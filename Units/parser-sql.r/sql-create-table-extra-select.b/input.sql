-- Taken from https://dev.mysql.com/doc/refman/5.7/en/create-table-select.html

-- test, a, b, c
CREATE TABLE test (a INT NOT NULL AUTO_INCREMENT,
		PRIMARY KEY (a), KEY(b))
		ENGINE=MyISAM SELECT b,c FROM test2;

