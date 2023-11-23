class ColumnPreferencesFrame : public QFrame
{
    Q_OBJECT
    void method0(void);

private slots:
    void slot0(void);
    void slot1(void);

public slots:
    void slot2(void);
    void slot3(void);

Q_SLOTS:
    void slot4(void);
    void slot5(void);

private:
  int field0;

protected:
  virtual void method1(void);

private Q_SLOTS:
  void slot6(void);
  void slot7(void);

public Q_SLOTS:
  void slot8(void);
  void slot9(void);

slots:
  void slot10(void);
  void slot11(void);

private:
  int field1;

signals:
  void signal0(void);
  void signal1(void);

private:
  int field2;

protected:
  virtual void method2(void);

Q_SIGNALS:
  void signal2(void);
  void signal3(void);

private:
  int field3;

protected:
  virtual void method3(void);

  Q_PROPERTY(QString text MEMBER m_text NOTIFY textChanged)
  Q_PROPERTY( bool ShowNonprinting READ showsNonprinting WRITE setShowsNonprinting )

private:
  int field4;

};
