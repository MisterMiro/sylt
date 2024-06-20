from flask import Flask, render_template, request
import subprocess
import sys
import os

app = Flask(__name__)

sylt_binary = "sylt_bin"
sylt_demos = "demo/"
timeout = 10

# converts 'example_file.sylt' to 'Example File'
def prettify_demo_name(file):
    return file.replace("_", " ").replace(".sylt", "").title()

# returns a list of all demos
def get_demo_list():
    demos = []
    for file in os.listdir(sylt_demos):
        demos.append((sylt_demos + file, prettify_demo_name(file)))

    demos.append(("", ""))
    demos.append(("stdlib.sylt", "Standard library"))
    demos.append(("tests.sylt", "Tests"))
    return demos

# loads the source code of a demo from file
def load_demo_code(file):
    if not file:
        return None
    
    with open(file, "r") as file:
        return file.read()

# launches a sylt instance and returns the output from stdout
def run_sylt_binary(args):
    cmd = ["./" + sylt_binary] + args
    try:
        sylt_process = subprocess.run(cmd, capture_output=True, timeout=timeout)
        return sylt_process.stdout.decode(sys.stdout.encoding)
    except subprocess.TimeoutExpired:
        return "Timed out (is there an infinite loop?)"

@app.route("/", methods=["GET"])
def index():
    demo = load_demo_code(request.args.get("load"))
    return render_template("index.html", demos=get_demo_list(), code=demo)

@app.route("/", methods=["POST"])
def index_submit():
    # get the source code from the input box
    code = request.form["code"]
    disassemble = request.form.get("disassemble")

    # save the code to a temporary file
    path = "web/tmp/" + request.host
    with open(path, "w", encoding="utf-8") as file:
        file.write(code)

    # launch sylt instance
    args = [path, "-v"]
    if (disassemble):
        args.append("-d")
    output = run_sylt_binary(args)    

    demos = get_demo_list()
    return render_template("index.html", demos=demos, code=code, disassemble=disassemble, output=output)
