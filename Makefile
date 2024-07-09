all: server client

server: compile_server start_server

client: compile_client start_client

compile_server:
	erlc -o ebin src/chat_supervisor.erl src/chat_server.erl src/chat_controller.erl

compile_client:
	erlc -o ebin src/chat_client.erl

start_server:
	erl -pa ebin

start_client:
	erl -pa ebin
