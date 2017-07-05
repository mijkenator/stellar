#!/usr/bin/python3

import requests

s = requests.Session()
url = "https://admin.stellarmakeover.com/_mijkweb/admin/service"

payload = {'request' : '{"type":"get_categories", "login":"twofatducks@gmail.com", "password":"56*yKF8Qe#zP"}'}
r = s.post(url, data=payload)
print(r.text)

url = "https://admin.stellarmakeover.com/_mijkweb/admin/contractor"
payload = {'request' : '{"type":"invite_contractor", "email":"mijkenator@mailinator.com"}'}
#payload = {'request' : '{"type":"get_contractors"}'}
r = s.post(url, data=payload)
print(r.text)

