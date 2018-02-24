rmdir /S /Q %~dp0android\app\src\main\assets\eavdb\

mklink /D %~dp0android\app\src\main\assets\eavdb %~dp0eavdb
pause