const express = require('express');
const fs = require('fs');
const app = express();
const port = 3000;

app.get('/', (req, res) => {
    var county = req.query.county;
    var state = req.query.state;

    fs.writeFile("County State.txt", county + ", " + state, function(err) {
        if(err) {
            return console.log(err);
        }
    
        console.log("New county file was saved.");
    });

    res.status(200).send(' ');
})

app.listen(port, () => console.log(`On the Move listening on port ${port}.`))