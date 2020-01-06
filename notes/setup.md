# Setting up Haskell and book resources

- Open your browser on the book website: [http://www.haskellcraft.com](http://www.haskellcraft.com)
- Follow the link to the to the Start page.
- If working on the campus computers, Haskell is already installed for you, so you can skip the first part about installing "Haskell Platform".
- Open up a terminal window, and navigate to the location you will use for this class (perhaps create a new directory with `mkdir` to store your work for the class).
- From the terminal, follow the three commands indicated in the Start page.
- From the `Craft3e-0...` directory, locate the `refresh.html` file and open it up in a web browser.
- Back in your terminal, start a Haskell interactive session with a particular module loaded via:

    ```bash
    ghci PicturesSVG
    ```
    You should see a prompt that looks like: `*PicturesSVG> `.
- In this prompt type the following commands, and look at the page after each. For each, think about what the line says and how that makes sense in the picture you see.

    ```haskell
    render horse
    render (flipV horse)
    render (flipV (flipH horse))
    render (horse `beside` horse)
    render (flipH horse `beside` flipV horse)
    render (horse `above` flipH horse)
    render (negative horse `beside` horse)
    ```
- End your interactive session by either typing `:exit` or by pressing `Ctrl-D`.
