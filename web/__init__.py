from flask import Flask, render_template, request
import subprocess
import sys
from os import listdir

app = Flask(__name__)

sylt_binary = "./sylt_bin"

def prettify_demo_name(file):
    return file.replace("_", " ").replace(".sylt", "").title()

def get_demo_list():
    filenames = listdir("demo/")

    demos = []
    for file in filenames:
        demos.append((file, prettify_demo_name(file)))
    return demos

def get_demo_code(file):
    demo = None
    if (file):
        with open("demo/" + file, "r") as file:
            demo = file.read()
    return demo

@app.route("/", methods=["GET"])
def index():
    demo = get_demo_code(request.args.get("load_demo"))
    return render_template("index.html", demos=get_demo_list(), demo=demo)

@app.route("/", methods=["POST"])
def index_run_code():
    # get the source code from the input box
    code = request.form["code"]
    #disassemble = request.form["disassemble"]
    disassemble = False

    # save the code to a temporary file
    path = "web/tmp/input.sylt"
    with open(path, "w", encoding="utf-8") as file:
        file.write(code)

    # set flags
    cmd = [sylt_binary, path, "-v"]
    if (disassemble):
        cmd.append("-d")

    # run the sylt CLI and get its output
    sylt = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    output = sylt.communicate()[0].decode(sys.stdout.encoding)

    demos = get_demo_list()
    demo = code
    return render_template("index.html", demos=demos, demo=demo, output=output)
