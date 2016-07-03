
class Cls:
    pass

VAR1 = Cls()

VAR2, VAR1.extra_member1 = 2, 1.1
VAR1.extra_member2 = 1.2
VAR3, VAR1.extra_member3, VAR4 = 3, 1.3, 4
