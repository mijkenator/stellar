#!/usr/bin/python3

import requests

s = requests.Session()
url = "http://52.76.131.184/_mijkweb/"

payload = {'request' : '{"type":"login", "login":"2279778@mail.ru", "password":"1815133", "as":"contractor"}'}
r = s.post(url, data=payload)
print(r.text)

url = "http://52.76.131.184/_mijkweb/contractor"
#payload = {'request' : '{"type":"signup", "login":"mijkenator@gmail.com", "password":"test", "refcode":"alala"}'}
#payload = {'request' : '{"type":"signup","login":"gelevanog@gmail.com","password":"1815133","refcode":"1ad54abc-6a03-4e0d-9b78-e84e40b80724"}'}
#payload = {'request' : '{"type":"get_details"}'}

payload = {'request' : '{"type":"set_details", "email":"2279778@mail.ru", "fname":"wer", "lname":"wer", "phone":"", "street":"", "apt":"", "city":"", "state":"", "cell_phone":"(852) 4564 5645", "bank_routing":"", "bank_account":""}'}
r = s.post(url, data=payload)
print(r.text)

