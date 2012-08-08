start erl -sname sail_web -pa ebin -pa deps/cowboy/ebin -pa deps/jsx/ebin -pa apps/sail_server/ebin -pa apps/sail_web/ebin -s sail_web_app -s reloader
sleep 5
"D:\Documents and Settings\psturc\Local Settings\Application Data\Google\Chrome\Application\chrome.exe" http://localhost:8080/static/index.html
