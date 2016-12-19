#!/usr/bin/python3

import requests

s = requests.Session()
#url = "http://127.0.0.1:8080"
url = "http://52.76.131.184/_mijkweb/"

payload = {'request' : '{"type":"login", "login":"mijkenator@gmail.com", "password":"test", "as":"admin"}'}
#payload = {'request' : '{"type":"login", "login":"mijkenator", "password":"test"}'}
#payload = {'request' : '{"type":"login", "login":"gelevanog@gmail.com", "password":"lalala", "as":"contractor"}'}
r = s.post(url, data=payload)
print(r.text)


payload = {'request' : '{"type":"get_orders", "uid":"18", "cid":"4"}'}
r = s.post(url+"admin/order/", data=payload)
print(r.text)

