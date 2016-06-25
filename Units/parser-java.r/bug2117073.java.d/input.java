public class AAssignmentExp {
  public AAssignmentExp(@SuppressWarnings("hiding") TAssign _token_,
                        @SuppressWarnings("hiding") PLvalue _lvalue_,
                        @SuppressWarnings("hiding") PExp _exp_) {
    setToken(_token_);
    setLvalue(_lvalue_);
    setExp(_exp_);
  }

  @Override void removeChild(@SuppressWarnings("unused") Node child) {
  }
}
