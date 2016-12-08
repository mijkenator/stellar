#!/usr/bin/python3

import requests

s = requests.Session()
#url = "http://127.0.0.1:8080"
url = "http://52.76.131.184/_mijkweb/user"

#payload = {'request' : '{"type":"signup", "login":"user2", "password":"test"}'}
#r = s.post(url, data=payload)
#print(r.text)

#payload = {'request' : '{"type":"restore_password", "login":"mijkenator@gmail.com"}'}
#r = s.post(url, data=payload)
#print(r.text)

#payload = {'request' : '{"type":"signup-confirm", "guid":"111111"}'}
#r = s.post(url, data=payload)
#print(r.text)

#exit(0)

payload = {'request' : '{"type":"restore-pwd-confirm", "guid":"111111", "password":"test123"}'}
r = s.post(url, data=payload)
print(r.text)
