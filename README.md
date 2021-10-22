# CSE 481 DS: Data Science Capstone

First, create a virtual environment (I used Python 3.8.12) and activate it using the following:

```
python3 -m venv ./venv
source ./venv/bin/activate
```
Note: the activation command is only for macOS/Unix, there is a different command for Windows

Then, install all of the required packages by running:

```
pip3 install -r requirements.txt
```

To use the virtual environment packages in a Jupyter notebook, we need to manually add the kernel by running:

```
python3 -m ipykernel install --name=venv
```

Whenever the virtual environment is activated and a Jupyter notebook is opened, you should be able to select the virtual environment created before (```venv```) as the kernel so that the packages in the virtual environment are used for the Jupyter notebook