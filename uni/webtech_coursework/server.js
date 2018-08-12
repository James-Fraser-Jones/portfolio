"use strict"; //enable strict mode

/* npm installs necessary:
npm install sqlite3
npm install mime-types
npm install mustache
npm install cookies
*/

const     http = require("http");               //http
const       fs = require("fs");                 //file system
const      sql = require("sqlite3").verbose();  //sqlite database, verbose gives better error messages
const     mime = require("mime-types");         //for converting file extensions to MIME types
const mustache = require("Mustache");           //for html templating
const  Cookies = require("cookies")             //for creating, sending and extracting cookies to remember sessions

//constant objects
const server = http.createServer();         //server object
const dataDb = new sql.Database("data.db"); //database object

//constant values
const OK = 200, NotFound = 404, BadType = 415, Error = 500; //http status codes
const port = 8080;

const siteDir = "./";
const publicDir = siteDir + "public/";

//============================================================================================
//code that runs on startup

start();

//============================================================================================
//callback functions and other functions

function start(){
  //if user or item tables don't exist, make them and also make "master, all, MASTER, 0" account which can see and act on behalf of all users, for testing purposes
  dataDb.all("select name from sqlite_master where type='table'", res);
  function res(err, tableInfo){
    if (tableInfo.length < 2){
      dataDb.serialize(initialise);
      function initialise(){
        dataDb.run("CREATE TABLE `users` (`id` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE, `username` TEXT NOT NULL, `password`	TEXT NOT NULL, `name` TEXT NOT NULL, `money` INTEGER NOT NULL)");
        dataDb.run("CREATE TABLE `items` (`id`	INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE, `owner` INTEGER NOT NULL, `description` TEXT NOT NULL, `price`	INTEGER NOT NULL)");
        dataDb.run('insert into users (username, password, name, money) values ("master", "all","Master",0)');
      }
    }
  }

  server.on("request", handler);                                //set callback function to respond to http requests
  server.listen(port, "localhost");                             //make server listen to requests from localhost, on the given port
  console.log("Server running at", "http://localhost:" + port); //acknowledge that server is running
}

//////////////////////Normal Request Handlers///////////////////////////

function handler(request, response){
  let url = request.url;

  let action = url.split("?")[1];
  if (action){
    return loginActionHandler(request, response, action);
  }

  if (url === "/"){ //make homepage accessible from root directory
    url = "/home";
  }

  if (!url.split(".")[1]){ //html file extensions need not be mentioned
    url = url + ".html";
  }

  fs.readFile(publicDir + url, ready);
  function ready(err, content){

    if (err){
      return fail(request, response);
    }

    //make sure cookie is not undefined if serving an html page because it needs to be inserted into template
    let cookie = "";
    let type = getType(url);
    if (type === "text/html"){
      let cookies = new Cookies(request, response);
      cookie = cookies.get("user");
      if (!cookie){
        cookie = "!none";
        cookies.set("user", cookie);
      }
    }

    //handle requests to market seperately since it requires more information from database
    if (url === "/market.html"){
      return marketHandler(request, response, content, cookie);
    }

    let typeHeader = { "Content-Type": type };
    response.writeHead(OK, typeHeader);
    if (type === "text/html"){
      let newContent = mustache.render(content.toString(), {username: cookie.split("&")[0]});
      response.write(newContent);
    }
    else{
      response.write(content);
    }
    response.end();
  }
}

function marketHandler(request, response, content, cookie){
  let user = cookie.split("&")[0];
  let filter = "";
  if (cookie !== "master&all"){
    filter = ' where username = "' + user + '"';
  }
  dataDb.all('select * from users' + filter, insertUserTable);
  function insertUserTable(err, userTable){
    dataDb.all('select items.id, name, description, price from items inner join users on items.owner = users.id', insertItemTable);
    function insertItemTable(err, itemTable){
      let newContent = mustache.render(content.toString(), {users: userTable, items: itemTable, username: user}); //use template with tables recieved from database to generate html
      let typeHeader = { "Content-Type": "text/html" };
      response.writeHead(OK, typeHeader);
      response.write(newContent);
      response.end();
    }
  }
}

function fail(request, response){
  let typeHeader = { "Content-Type": "text/plain" };
  response.writeHead(302,  {Location: "/error"}); //redirect to error page
  response.write("File Not Found");
  response.end();
}

function getType(url){
  let type = url.substring(url.lastIndexOf(".") + 1);
  return mime.lookup(type);
}

//////////////////////Action Request Handlers///////////////////////////

function loginActionHandler(request, response, action){
  //login actions, these always contain username and password information, hence don't need to be validated with a session cookie
  let info = action.split("&");

  if (info[0] === "signin" && info.length === 3){
    return signIn(info[1], info[2], request, response);
  }
  else if (info[0] === "signup" && info.length === 4){
    return signUp(info[1], info[2], info[3], request, response);
  }
  else if (info[0] === "signout" && info.length === 1){
    return signOut(request, response);
  }
  else if (info[0] === "deleteaccount" && info.length === 3){
    return deleteAccount(info[1], info[2], request, response);
  }
  else{
    return marketActionHandler(request, response, info);
  }
}

//TODO: modify this so that it denies attempts to change things when the username and password in the cookie don't match the username and password given by sql when searching for the user with the given userid
function marketActionHandler(request, response, info){
  //market actions, these need to be validated with a session cookie, master account can act on behalf of anybody

  //get cookie info
  let cookies = new Cookies(request, response);
  let cookieInfo = cookies.get("user").split("&");

  //if cookie contains username and password
  if (cookieInfo.length === 2){
    let username = cookieInfo[0];
    let password = cookieInfo[1];

    let userId = Number(info[1]);

    dataDb.all("select username, password from users where id = " + userId, res); //get user info from database
    function res(err, userInfo){
      if (userInfo.length > 0){
        if ((username === userInfo[0].username && password === userInfo[0].password) || (username === "master" && password === "all")){
          //at this point the action is allowed
          if (info[0] === "buy" && info.length === 3){
            buy(Number(info[1]), Number(info[2]));
          }
          else if (info[0] === "removeitem" && info.length === 3){
            removeItem(Number(info[1]), Number(info[2]));
          }
          else if (info[0] === "additem" && info.length === 4){
            addItem(Number(info[1]), info[2], Number(info[3]));
          }
          else if (info[0] === "addfunds" && info.length === 3){
            addFunds(Number(info[1]), Number(info[2]));
          }
          else if (info[0] === "removefunds" && info.length === 3){
            removeFunds(Number(info[1]), Number(info[2]));
          }
        }
      }
    }
  }

  let typeHeader = { "Content-Type": "text/plain" }; //default response to calling an action
  response.writeHead(OK, typeHeader);
  response.write("action acknowledged");
  response.end();
}

//////////////////////Market Actions///////////////////////////

function buy(buyerId, itemId){
  dataDb.all("select owner, price from items where id = " + itemId, res1); //get info about item
  function res1(err, itemInfo){
    dataDb.all("select money from users where id = " + buyerId, res2); //get info about buyer
    function res2(err, userInfo){
      let    money = userInfo[0].money;
      let sellerId = itemInfo[0].owner;
      let    price = itemInfo[0].price;

      if (money >= price && buyerId !== sellerId){ //does this person have enough money to buy the object?
        dataDb.serialize(transact);
        function transact(){
          dataDb.run("update users set money = money + " + price + " where id = " + sellerId);
          dataDb.run("update users set money = money - " + price + " where id = " + buyerId);
          dataDb.run("update items set owner = " + buyerId + " where id = " + itemId);
        }
      }
    }
  }
}

function addItem(ownerId, desc, price){
  let newDesc = desc.split("%20").join(" ") //correct the url encoding for spaces
  if(ownerId >= 0 && newDesc.length > 0 && price > 0){
    dataDb.run('insert into items (owner, description, price) values (' + ownerId + ', "' + newDesc + '", ' + price + ')');
  }
}

function removeItem(ownerId, itemId){
  dataDb.all("select owner from items where id = " + itemId, res1); //get info about item
  function res1(err, itemInfo){
    let realOwnerId = itemInfo[0].owner;
    if (ownerId === realOwnerId){ //does this person own the item they're attempting to remove?
      dataDb.run("delete from items where id = " + itemId);
    }
  }
}

function addFunds(userId, amount){
  dataDb.run("update users set money = money + " + amount + " where id = " + userId);
}

function removeFunds(userId, amount){
  dataDb.all("select money from users where id = " + userId, res); //get info about user
  function res(err, userInfo){
    let money = userInfo[0].money;
    if (amount > money) amount = money; //prevent user from taking out more money than they started with
    dataDb.run("update users set money = money - " + amount + " where id = " + userId);
  }
}

//////////////////////Login Actions///////////////////////////

function signIn(username, password, request, response){
  dataDb.all('select password from users where username = "' + username + '"', res); //get info about user
  function res(err, userInfo){
    if (userInfo.length > 0 && userInfo[0].password === password){ //is the password correct?
      let cookies = new Cookies(request, response);
      cookies.set("user", username + "&" + password);
    }
    let typeHeader = { "Content-Type": "text/plain" }; //default response to calling an action
    response.writeHead(OK, typeHeader);
    response.write("action acknowledged");
    response.end();
  }
}

function signUp(username, password, name, request, response){
  dataDb.all('select id from users where username = "' + username + '"', res); //get info about user
  function res(err, userInfo){
    if (userInfo.length === 0 && username.length > 0 && password.length > 0 && name.length > 0){ //ensure there isn't already a user with the given username
      dataDb.run('insert into users (username, password, name, money) values ("' + username + '", "' + password + '", "' + name + '", ' + 0 + ')');
    }
    signIn(username, password, request, response);
  }
}

function signOut(request, response){
  let cookies = new Cookies(request, response);
  let cookie = "!none";
  cookies.set("user", cookie);

  let typeHeader = { "Content-Type": "text/plain" }; //default response to calling an action
  response.writeHead(OK, typeHeader);
  response.write("action acknowledged");
  response.end();
}

function deleteAccount(username, password, request, response){
  dataDb.all('select id, password from users where username = "' + username + '"', res); //get info about user
  function res(err, userInfo){
    if (userInfo.length > 0 && userInfo[0].password === password){ //is the password correct?
      let userId = userInfo[0].id;
      dataDb.serialize(removal);
      function removal(){
        dataDb.run("delete from items where owner = " + userId);
        dataDb.run("delete from users where id = " + userId);
      }
      let cookies = new Cookies(request, response); //sign out after deleting account
      let cookie = "!none";
      cookies.set("user", cookie);
    }
    let typeHeader = { "Content-Type": "text/plain" }; //default response to calling an action
    response.writeHead(OK, typeHeader);
    response.write("action acknowledged");
    response.end();
  }
}
