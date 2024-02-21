# indent-s-expr

Dumb-as-rocks S-expression indenter.

## Example

```rs
use indent_s_expr::indent_sexp;

indent_sexp("(+ (alpha / beta) (gamma * delta))", 2);
/*

(
  +
  (
    alpha
    /
    beta
  )
  (
    gamma
    *
    delta
  )
)

*/
```
