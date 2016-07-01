#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_MATRIX(X);
  DATA_VECTOR(Y);
  DATA_VECTOR(offset);
  DATA_IVECTOR(s_i);
  DATA_IVECTOR(c_i);
  DATA_IVECTOR(s_c_i);

  PARAMETER(C);
  PARAMETER_VECTOR(B);
  PARAMETER_VECTOR(B_s);
  PARAMETER(log_SD_s);
  PARAMETER_VECTOR(B_c);
  PARAMETER(log_SD_c);

  max_parallel_regions = omp_get_max_threads(); 

  parallel_accumulator<Type> jnll(this);

  // Type jnll = 0.0;

  vector<Type> lin_pred = C + X*B;
  for (int i = 0; i < Y.size(); ++i)
    lin_pred(i) += B_s(s_i(i)) + B_c(c_i(i));

  vector<Type> pred_count = exp(lin_pred) * offset;

  // vector<Type> var = pred_count + (pred_count*pred_count)/exp(log_theta);

  for (int i = 0; i < Y.size(); ++i)
    jnll -= dpois(Y(i), pred_count(i), true);
  for (int i = 0; i < B_s.size(); ++i)
    jnll -= dnorm(B_s(i), Type(0), exp(log_SD_s), true);
  for (int i = 0; i < B_c.size(); ++i)
    jnll -= dnorm(B_c(i), Type(0), exp(log_SD_c), true);

  REPORT(pred_count);

  return jnll;
}
