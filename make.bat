DEL *.dump
DEL .\ebin\*.beam
COPY .\lib\*.beam .\ebin\
"C:\Program Files (x86)\erl5.7.4\bin\erl.exe" -make
"C:\Program Files (x86)\erl5.7.4\bin\werl.exe" -pa ./ebin -pa ./src -pa ./tests -run querly_tests run_all -run querly_db_tests run_all
exit /B