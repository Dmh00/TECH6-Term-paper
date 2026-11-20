
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.preprocessing import PolynomialFeatures, StandardScaler
from sklearn.linear_model import LinearRegression, Lasso
from sklearn.model_selection import train_test_split, GridSearchCV, KFold
from sklearn.pipeline import Pipeline
from sklearn.metrics import r2_score, root_mean_squared_error

RANDOM_STATE = 1234
SCORING = 'neg_root_mean_squared_error'
NUM_JOBS = 4

# load data and print the number of municipalities
df_all = pd.read_stata('regdata0.dta',convert_categoricals=False)
print(f"Number of municipalities: {len(df_all)}")

# outcome: share of women with high school education (2000), age 15-20
target = 'hischshr1520f'
# 7 election predictors
pred7 = ['vshr_islam1994','partycount','lpop1994','merkezi','merkezp','subbuyuk','buyuk']
# 7 election predictors + 7 census predictors (1990)
pred14 = pred7 + ['c90lpop','c90ageshr19','c90ageshr60','c90sexr','c90mrf','c90hischshr1520f','c90hischshr1520m']


# Define four specifications: A, B, BC, C
# A: 7 predictors, degree 1, B: 7 predictors, degree 1 and 2,
# BC: 7 predictors on smaller sample, degree 1 and 2, C: 14 predictors, degree 1 and 2
specs = {'A':(pred7,[1],pred7), 'B':(pred7,[1,2],pred7),
         'BC':(pred7,[1,2],pred14),'C':(pred14,[1,2],pred14)}

# Split 1234 is the workshop split
# the other five are a robustness check
seeds = [RANDOM_STATE,1,2,3,4,5]

def make_model(estimator):
    """
    Build a polynomial regression pipeline.
    Parameters
    ---------
    estimator : object
        Regression estimator used after preprocessing

    Returns
    ------
    Unfitted pipeline containing preprocessing and regression
    """
    pipe = Pipeline(
        [
            # standardize inputs
            ('input_scale', StandardScaler()),
            # add squares and interactions
            ('poly', PolynomialFeatures(include_bias=False)),
            # standardize expanded inputs
            ('scale', StandardScaler()),
            # OLS or Lasso regression
            ('model', estimator),
        ]
    )
    return pipe


def plot_validation_curves(search, model_name):
    """
    Plot validation RMSE against alpha for each polynomial degree.

    Parameters
    ----------
    search : object
        Fitted grid search over polynomial degree and model alpha
    model_name : str
        Model name used in the plot title
    """
    # search results as table
    df_results = pd.DataFrame(search.cv_results_)
    degree_col = 'param_poly__degree'
    colors = ['#1f78b4','#e31a1c', '#33a02c']

    _, ax = plt.subplots(figsize=(5.5, 3.2), layout='constrained')

    for i, (degree, df_group) in enumerate(df_results.groupby(degree_col)):
        df_group = df_group.sort_values('param_model__alpha')
        alphas = df_group['param_model__alpha']
        # Scores are negative RMSE, so change sign
        rmse = -df_group['mean_test_score']
        ax.plot(alphas, rmse, c=colors[i], label=f'Degree {degree}')

    # best combination of polynomial degree and alpha
    best = df_results.loc[search.best_index_]
    ax.scatter(
        best['param_model__alpha'], -best['mean_test_score'], c='black', zorder=3
    )
    ax.set_xscale('log')
    ax.set_xlabel(r'Regularization strength $\alpha$')
    ax.set_ylabel('CV RMSE in completion share')
    ax.set_title(f'Validation curve for {model_name}')
    ax.legend()

# Splitter used for cross-validation
cv = KFold(n_splits=5,shuffle=True,random_state=RANDOM_STATE)

# Define the candidate alphas for Lasso regression
lasso_alphas = np.logspace(-4,-1,num=100)
rows = []

# loop over the four versions (A,B,BC,C)
for spec in specs:
    # Get predictors, polynomial degrees, and sample variables
    predictors,degrees,sample_vars = specs[spec]
    print(f"\nVersion {spec}: {len(predictors)} predictors, degrees {degrees}")
    # Drop observations with missing values
    df = df_all[sample_vars+[target]].dropna()
    print(f"Number of observations in common sample: {len(df)}")

    # loop over the six random seeds
    for seed in seeds:
        # split data into training and test samples
        df_train,df_test = train_test_split(df,test_size=0.5,random_state=seed)
        X_train,y_train = df_train[predictors],df_train[target]
        X_test,y_test = df_test[predictors],df_test[target]

        # OLS regression
        param_grid = {'poly__degree': degrees}
        pipe_ols = make_model(LinearRegression())
        ols_cv = GridSearchCV(pipe_ols,
                              param_grid=param_grid,
                              cv=cv,
                              scoring=SCORING,
                              refit=True,
                              n_jobs=NUM_JOBS)
        ols_cv.fit(X_train, y_train)

        # Lasso regression
        lasso_param_grid = {'poly__degree': degrees, 'model__alpha': lasso_alphas}
        lasso_model = Lasso(
            max_iter=1_000_000,
            tol=1e-6,
            precompute=True,
            selection='random',
            random_state=RANDOM_STATE,
        )
        pipe_lasso = make_model(lasso_model)

        # Define Gridsearch for Lasso regression
        lasso_cv = GridSearchCV(pipe_lasso,
                                param_grid=lasso_param_grid,
                                cv=cv,
                                scoring=SCORING,
                                refit=True,
                                n_jobs=NUM_JOBS)

        # Run cross-validation
        lasso_cv.fit(X_train,y_train)

        # store main split of version C for figure B2 and 1990-only model
        if spec == 'C' and seed == RANDOM_STATE:
            alpha_lasso = lasso_cv.best_params_['model__alpha']
            lasso_cv_C,df_train_C, df_test_C = lasso_cv,df_train,df_test
            # check if best alpha is at edge of grid
            at_edge = alpha_lasso in (lasso_alphas[0],lasso_alphas[-1])
            print(f"Lasso alpha {alpha_lasso:.4g}")
            print(f"Grid {lasso_alphas[0]} to {lasso_alphas[-1]}, at endpoint: {at_edge}")

        # Compare the models on the test sample
        for model_name, search in [("OLS",ols_cv),("Lasso",lasso_cv)]:
            best = search.best_estimator_
            model = best.named_steps['model']
            y_pred = best.predict(X_test)
            rows.append(
                {
                    'Version': spec,
                    'Seed': seed,
                    'Model': model_name,
                    'Degree': search.best_params_['poly__degree'],
                    'Alpha': search.best_params_.get('model__alpha', np.nan),
                    '# Coefs': len(model.coef_),
                    '# Non-zero coefs': np.count_nonzero(np.abs(model.coef_) > 1e-8),
                    'CV RMSE': -search.best_score_,
                    'Test RMSE': root_mean_squared_error(y_test, y_pred),
                    'Test R2': r2_score(y_test, y_pred),
                    'N train': len(df_train),
                    'N test': len(df_test),
                })

        # Benchmark: predict the training mean for every municipality
        y_mean = np.full(len(y_test),y_train.mean())
        rows.append(
            {
                'Version': spec,
                'Seed': seed,
                'Model': 'Predict the mean',
                'Test RMSE': root_mean_squared_error(y_test, y_mean),
                'Test R2': r2_score(y_test, y_mean),
                'N train': len(df_train),
                'N test': len(df_test),
            })

df_cv = pd.DataFrame(rows)
df_cv.to_csv('outputs/results_all.csv',index=False)

# Figure B2: Lasso validation curve (version C)
plot_validation_curves(lasso_cv_C,'Lasso (with 1990 census)')
plt.savefig('outputs/figureB2_lasso_validation_curve.png',dpi=200)

# Version C: how much does the 1990 female outcome alone predict?
# OLS on the 1990 female outcome only
ols_1990 = LinearRegression().fit(df_train_C[['c90hischshr1520f']],
                                  df_train_C[target])

y_pred = ols_1990.predict(df_test_C[['c90hischshr1520f']])
print(f'\nVersion C sample OLS on the 1990 female outcome only: '
      f'test RMSE {root_mean_squared_error(df_test_C[target], y_pred):.4f}, test R2 {r2_score(df_test_C[target], y_pred):.3f}')

# Save results to csv
pd.DataFrame([{'Version': 'C', 'Model': 'OLS, 1990 female outcome only',
               'Test RMSE': root_mean_squared_error(df_test_C[target], y_pred),
               'Test R2': r2_score(df_test_C[target], y_pred)}]).to_csv('outputs/results_1990_outcome_only.csv', index=False)