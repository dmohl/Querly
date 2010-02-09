To get started with querly, simply type the following:

- querly_client:sql_query("select * from person where idno = 1").

Note: This query assumes that querly is pointing to a CouchDB instance that contains a database named person with an attribute named idno and at least one document with an idno = 1.