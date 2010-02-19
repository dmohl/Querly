DEL *.dump
DEL .\ebin\*.beam
COPY .\lib\*.* .\ebin\
"C:\Program Files (x86)\erl5.7.4\bin\erl.exe" -make
"C:\Program Files (x86)\erl5.7.4\bin\werl.exe" -pa ./ebin -pa ./src -pa ./tests -run querly_tests run_all -run querly_db_tests run_all -run querly_client_tests run_all -run querly_rabbitmq_tests run_all -run querly_helper_tests run_all
exit /B