# Gerenciamento-de-Alunos
A small program written in Haskell for a college assignment.


The program has been divided into modules to facilitate code readability and organization. Before executing the command to create the executable, it is necessary to create a folder called "build." All executables generated by the compilation will be stored inside this folder, preventing the main folder from becoming cluttered.

To compile the program, execute the following command:
ghc -package directory -package deepseq main.hs -o main.exe -outputdir build/

The imports of the directory and deepseq packages are essential for the correct functioning of the program. 
After compilation, run the generated .exe file (on Windows) or the corresponding executable file for your operating system. 

If you are running on Linux, you may need to grant execution permission for the file with the following command:
chmod +x build/main

Then, execute it with the command:
./main
If the main file is in the build folder, run it with:
./build/main
