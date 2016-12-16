#!/usr/bin/python3

import requests

s = requests.Session()
url = "http://52.76.131.184/_mijkweb/admin/service"

payload = {'request' : '{"type":"get_categories", "login":"mijkenator@gmail.com", "password":"test"}'}
r = s.post(url, data=payload)
print(r.text)

url = "http://52.76.131.184/_mijkweb/admin/contractor"
#payload = {'request' : '{"type":"invite_contractor", "email":"mijkenator@gmail.com"}'}
payload = {'request' : '{"type":"get_contractors"}'}
r = s.post(url, data=payload)
print(r.text)

