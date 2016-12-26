#!/usr/bin/python3

import requests

s = requests.Session()
#url = "http://127.0.0.1:8080"
url = "http://52.76.131.184/_mijkweb/user"

payload = {'request' : '{"type":"signup", "login":"mijkenator@gmail.com", "password":"test"}'}
r = s.post(url, data=payload)
print(r.text)

#exit(0)

#payload = {'request' : '{"type":"restore_password", "login":"mijkenator@gmail.com"}'}
#r = s.post(url, data=payload)
#print(r.text)

#payload = {'request' : '{"type":"signup-confirm", "guid":"111111"}'}
#r = s.post(url, data=payload)
#print(r.text)

#exit(0)

#payload = {'request' : '{"type":"restore-pwd-confirm", "guid":"111111", "password":"test123"}'}
#payload = {'request' : '{"type":"delete", "login":"mijkenator@gmail.com", "password":"test123"}'}
payload = {'request' : '{"type":"get_details", "login":"mijkenator@gmail.com", "password":"test"}'}
r = s.post(url, data=payload)
print(r.text)

payload = {'request' : '{"type":"set_details", "name":"1", "street":"2", "apt":"3", "zip":"4", "city":"5", "state":"6", "phone":"7"}'}
r = s.post(url, data=payload)
print(r.text)

payload = {'request' : '{"type":"get_details", "login":"mijkenator@gmail.com", "password":"test"}'}
r = s.post(url, data=payload)
print(r.text)

#payload = {'request' : '{"type":"create_order", "contractor_id":4, "service_id":3, "service_ontime":"2016-12-31 12:00:00",\
#                         "number_of_services":1, "number_of_contractors":1, "cost":99}'}
payload = {'request' : '{"type":"create_order","service_id":"3","service_ontime":"2016-12-26 12:15:00","number_of_services":1,"number_of_contractors":1,"cost":9}'}
r = s.post(url, data=payload)
print(r.text)

payload = {'request' : '{"type":"get_orders"}'}
r = s.post(url, data=payload)
print(r.text)
