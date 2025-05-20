# A variable with the nightly version from yesterday:
VERSION="$(date -d yesterday +0.0.0-0.nightly.%Y.%m.%d)"

FILENAME=carbon_toolchain-${VERSION}.tar.gz

# Get the release
if [ ! -e "$FILENAME" ] 
then 
    wget https://github.com/carbon-language/carbon-lang/releases/download/v${VERSION}/carbon_toolchain-${VERSION}.tar.gz
fi

# Unpack the toolchain:
tar -xvf "$FILENAME" 

# Create a simple Carbon source file:
echo "import Core library \"io\"; fn Run() { Core.Print(42); }" > forty_two.carbon

# Compile to an object file:
./carbon_toolchain-${VERSION}/bin/carbon compile \
    --output=./bin/forty_two.o forty_two.carbon

# Install minimal system libraries used for linking. Note that installing `gcc`
# or `g++` for compiling C/C++ code with GCC will also be sufficient, these are
# just the specific system libraries Carbon linking still uses.
sudo apt install libgcc-11-dev

# Link to an executable:
./carbon_toolchain-${VERSION}/bin/carbon link \
    --output=./bin/forty_two ./bin/forty_two.o

# Run it:
./bin/forty_two
