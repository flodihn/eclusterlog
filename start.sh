erl -config config/eclusterlog.config -pa ebin -pa deps/*/ebin -pa deps/*/deps/*/ebin +K true +P 1000000 -env ERL_MAX_PORTS 65535 -sname ecluster_srv -s eclusterlog init $1
