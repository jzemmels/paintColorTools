# paintColorTools
A shiny application to discover and compare paint colors across a variety of brands.
The app is available here: jzemmels.shinyapps.io/paintcolortools/.

After moving to a new townhouse that was in desparate need of painting, I became frustrated at how difficult it was to compare paint colors across brands.
Many brands use propietary coding schemes to uniquely identify their paint colors.
I stumbled across a data set available at [Converting Colors](https://convertingcolors.com/lists.html) featuring the Hex and RGB codes associated with paint colors for a variety of brands.
I was inspired to create an application that allows one to select a paint color from one brand and see "similar" (measured by Euclidean distance in RGB space) colors from other brands.

I also created a tool to associate colors in a user-uploaded image to similar paint colors.
The "Image to Palette" tab uses a clustering algorithm to identify regions in RGB space of similar and highly-dense colors.
From these clusters a representative palette is returned along with similar paint colors.
The "Image Color Picker" tab allows the user to select a specific pixel in the image and find similar paint colors.
