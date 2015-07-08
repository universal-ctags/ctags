struct Base {
  virtual void baz() const throw() = 0;
};

struct Foo final : public Base {
  static constexpr auto bar() noexcept { return 1; }
  virtual void baz() const throw() final override;
};
