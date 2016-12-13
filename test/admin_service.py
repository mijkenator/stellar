#!/usr/bin/python3

import requests

s = requests.Session()
#url = "http://127.0.0.1:8080"
url = "http://52.76.131.184/_mijkweb/admin/service"

payload = {'request' : '{"type":"get_categories", "login":"mijkenator@gmail.com", "password":"test"}'}
r = s.post(url, data=payload)
print(r.text)

exit(0)

