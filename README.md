# Final Project

## How to run the code
Run main.r file

## Folder structure
1. [folder] data: raw data are in this folder in xlsx form.

2. [file] main.r : main code file that runs all the code for this project

   a) If you don't have the renv package installed, run the following command first:

    ```R
    install.packages("renv")
     ```

   b) You have to make "apikeys.r" file in the "FINALPROJECT" folder and type in

    ```R
    fred_api_key <- "(your api key)"
    ```
    where (your api key) is your own api key. 
    You will need to get your own API key from FRED (https://fred.stlouisfed.org/docs/api/api_key.html).

3. [folder] forlatex: regression result tables are exported into this folder latex text form.

4. [folder] graph: jpeg graph files are exported into this folder

5. [folder] renv: it has files needed to create renv environment

6. [folder] tests: it has files for unit tests
