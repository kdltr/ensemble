#!/bin/sh

if `test -n "$XDG_CONFIG_HOME"`; then
    config_dir=$XDG_CONFIG_HOME
else
    config_dir=$HOME/.config
fi

old_db=$config_dir/ensemble.dat
new_db=$config_dir/ensemble/default

mkdir -p $new_db

cat <<EOF | chicken-sqlite3 $old_db >$new_db/credentials
select printf('(%s . %s)',key,value) from config where key in ("mxid", "server-uri", "access-token");
EOF
