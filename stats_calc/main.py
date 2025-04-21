from js import document, window, Uint8Array
from pyodide.ffi.wrappers import add_event_listener
import io
import csv

async def upload_file_and_show(e):
	file_list = e.target.files
	first_item = file_list.item(0)

	my_bytes: bytes = await get_bytes_from_file(first_item)
	file = io.StringIO(my_bytes.decode('utf-8'))
	for line in csv.DictReader(file):
		print(line)

async def get_bytes_from_file(file):
	array_buf = await file.arrayBuffer()
	return array_buf.to_bytes()

add_event_listener(document.getElementById("file-upload"), "change", upload_file_and_show)
