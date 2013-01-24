erlang-http-proxy-async
=======================

A http proxy written by erlang, use to provide multi http requests for php client.

Request:
curl -i 127.0.0.1:6907/ehpa -d "url=http://baidu.com&url=http://baidu.com&timeout=5000"

Response:
{"result":["<html>\n<meta http-equiv=\"refresh\" content=\"0;url=http://www.baidu.com/\">\n</html>\n","<html>\n<meta http-equiv=\"refresh\" content=\"0;url=http://www.baidu.com/\">\n</html>\n"]}