#!/usr/bin/python3

import requests

s = requests.Session()
#url = "http://127.0.0.1:8080"
url = "http://52.76.131.184/_mijkweb/admin/user"

payload = {'request' : '{"type":"send_support_message","fname":"fname","lname":"lname","email":"e","phone":"p","message":"m","as":"press"}'}
r = s.post(url, data=payload)
print(r.text)

