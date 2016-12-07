#!/usr/bin/python3

import requests

payload = {'request' : '{"type":"auth", "login":"mijkenator", "password":"test"}'}
r = requests.post("http://127.0.0.1:8080", data=payload)

print(r.text)
