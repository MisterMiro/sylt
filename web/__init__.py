from flask import Flask, render_template, request
import subprocess
import sys

app = Flask(__name__)

sylt_binary = "./sylt_bin"

@app.route("/")
def index():
    return render_template("index.html")

@app.route("/", methods=['POST'])
def my_form_post():
    # get the source code from the input box
    code = request.form["code"]

    # save the code to a temporary file
    path = "input.sylt"
    with open(path, "w", encoding="utf-8") as file:
        file.write(code)

    # run the sylt CLI and get its output
    sylt = subprocess.Popen([sylt_binary, path], stdout=subprocess.PIPE)
    output = sylt.communicate()[0].decode(sys.stdout.encoding)
    return render_template("index.html", output=output)
