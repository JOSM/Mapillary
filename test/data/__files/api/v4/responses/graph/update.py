#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Update responses with current "live" responses from Mapillary.
"""

import os
import re
import json
import requests
from typing import List

# From cat ../../../../../../../src/main/java/org/openstreetmap/josm/plugins/mapillary/utils/MapillaryURL.java| grep ACCESS_ID
MAPILLARY_API_KEY = "MLY|4223665974375089|d62822dd792b6a823d0794ef26450398"
MAPILLARY_API_ROOT = "https://graph.mapillary.com/"
MAPILLARY_HEADERS = {"Authorization": f"OAuth {MAPILLARY_API_KEY}"}

ITEM_RE = re.compile(r"[0-9]+")
def update_item(root: str, files: List[str]):
  for file in files:
    if ITEM_RE.search(file):
      fields = {}
      with open(os.path.join(root, file)) as fh:
        json_data = json.load(fh)
        fields = [key for key in json_data]
      params = {"fields": ",".join(fields)}
      request = requests.get(MAPILLARY_API_ROOT + file.replace(".json", ""), params=params, headers=MAPILLARY_HEADERS)

def update_image_ids(root: str, files: List[str]):
  for file in files:
    params = {"sequence_id": file.replace(".json", ""), "fields": "id"}
    request = requests.get(MAPILLARY_API_ROOT + "image_ids", params=params, headers=MAPILLARY_HEADERS)
    json_data = request.json()
    with open(os.path.join(root, file), "w") as fh:
      json.dump(json_data, fh, indent=2)

def update_files():
  for root, dirs, files in os.walk("."):
    if "venv" in root:
      continue
    print(root)
    json_files: List[str] = [f for f in files if f.endswith(".json")]
    if "image_ids" in root:
      update_image_ids(root, files)
    elif len(root) <= 1:
      update_item(root, files)

    print(json_files)

if __name__ == "__main__":
  update_files()
