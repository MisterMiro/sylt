from flask import Flask, render_template, request
import subprocess
import sys
from os import listdir

app = Flask(__name__)

sylt_binary = "sylt_bin"
sylt_demos = "demo/"

# converts 'example_file.sylt' to 'Example File'
def prettify_demo_name(file):
    return file.replace("_", " ").replace(".sylt", "").title()

# returns a list of all demos
def get_demo_list():
    demos = []
    for file in listdir(sylt_demos):
        demos.append((sylt_demos + file, prettify_demo_name(file)))

    demos.append(("", ""))
    demos.append(("tests.sylt", prettify_demo_name("tests.sylt")))
    demos.append(("stdlib.sylt", prettify_demo_name("stdlib.sylt")))
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
    ##sylt_process = subprocess.Popen(cmd, stdout=subprocess.PIPE)
    ##return sylt_process.communicate()[0].decode(sys.stdout.encoding)
    sylt_process = subprocess.run(cmd, capture_output=True)
    return sylt_process.stdout.decode(sys.stdout.encoding)


# main page
@app.route("/", methods=["GET"])
def index():
    demo = load_demo_code(request.args.get("load_demo"))
    return render_template("index.html", demos=get_demo_list(), code=demo)

# main page after submitting code to run
@app.route("/", methods=["POST"])
def index_run_code():
    # get the source code from the input box
    code = request.form["code"]
    disassemble = request.form.get("disassemble")

    # save the code to a temporary file
    path = "web/tmp/input.sylt"
    with open(path, "w", encoding="utf-8") as file:
        file.write(code)

    # launch sylt instance
    args = [path, "-v"]
    if (disassemble):
        args.append("-d")
    output = run_sylt_binary(args)    

    demos = get_demo_list()
    return render_template("index.html", demos=demos, code=code, disassemble=disassemble, output=output)
