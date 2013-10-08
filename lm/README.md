Scripts for building a language model with KenLM 
(http://kheafield.com/code/kenlm/) and a little script to start an HTTP server
for making queries to the LM.

To install, run `install.sh`. This will download and build KenLM. Notes:
* You will need Boost libraries installed and in a standard location.
* The script will try to install the KenLM python module (necessary for the
HTTP server). If you do not have permissions to install the module, this will
fail. To manually install the python module, `cd kenlm` and then 
`sudo python setup.py install`.

To create a language model, run `build_lm.sh text` where `text` is a file
with one tokenized sentence per line.

To start the server, run `python server.py port lm` where `port` is the HTTP
server port to start, and `lm` is the path to the language model (either in
`*.arpa` format or `*.binary` format; see KenLM docs for more info).

The server should be available at `http://localhost:8080` (or whatever you
chose for the port). The URL for making a query is 
`http://localhost:8080/score?q=<encoded sentences>` where `<encoded sentences>`
are URL-encoded sentences (e.g. `hello world` => `hello%20world`), each 
sentence separated by a `|` symbol. The server will return one log-probability
per line in plain text.
