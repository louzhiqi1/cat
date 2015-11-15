set NODE=game@localhost
set SMP=auto
set ERL_PROCESSES=102400
set TMPDIR=tmp

cd ..
md %TMPDIR%
werl +P %ERL_PROCESSES% -smp %SMP% -pa ebin ebin/deps src src/db --name %NODE% -kernel error_logger tty -s game start
pause
