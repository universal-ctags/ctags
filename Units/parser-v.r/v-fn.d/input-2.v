// generics

fn new_repo[T](db DB) Repo[T] {
    return Repo[T]{db: db}
}

pub fn (r Repo[T]) find_by_id(id int) ?T {
    table_name := T.name // in this example getting the name of the type gives us the table name
    return r.db.query_one[T]('select * from ${table_name} where id = ?', id)
}
