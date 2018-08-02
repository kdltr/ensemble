select printf("(");
select printf('(%s . %s)',key,value) from config where key in ("mxid", "server-uri", "access-token");
select printf(")");
