#!/usr/bin/python3

import requests

s = requests.Session()
#url = "http://127.0.0.1:8080"
url = "http://52.76.131.184/_mijkweb/contractor"

#payload = {'request' : '{"type":"signup", "login":"mijkenator@gmail.com", "password":"test", "refcode":"alala"}'}
payload = {'request' : '{"type":"signup","login":"gelevanog@gmail.com","password":"1815133","refcode":"1ad54abc-6a03-4e0d-9b78-e84e40b80724"}'}
r = s.post(url, data=payload)
print(r.text)

