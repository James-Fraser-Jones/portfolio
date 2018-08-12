"use strict"
addEventListener('load', start);

function start(){
  document.getElementById("signinButton").addEventListener("click", signin);
  document.getElementById("signupButton").addEventListener("click", signup);
  document.getElementById("signoutButton").addEventListener("click", signout);
  document.getElementById("deleteaccountButton").addEventListener("click", deleteaccount);

  var userInput = document.getElementById("userInput");
  var passInput = document.getElementById("passInput");
  var nameInput = document.getElementById("nameInput");
  userInput.value = "";
  passInput.value = "";
  nameInput.value = "";
}

function sendAction(url){
  let x = new XMLHttpRequest();
  x.onreadystatechange = receive;
  x.open("GET", url, true);
  x.send();

  function receive(){
    if (this.readyState != XMLHttpRequest.DONE) return;
    location.reload();
  }
}

function signin(){
  let username = userInput.value;
  let password = passInput.value;
  if (username.length > 0 && password.length > 0){
    let url = "http://localhost:8080/?signin&" + username + "&" + password;
    sendAction(url);
  }
}

function signup(){
  let username = userInput.value;
  let password = passInput.value;
  let profilename = nameInput.value;
  if (username.length > 0 && password.length > 0){
    let url = "http://localhost:8080/?signup&" + username + "&" + password + "&" + profilename;
    sendAction(url);
  }
}

function signout(){
  let url = "http://localhost:8080/?signout";
  sendAction(url);
}

function deleteaccount(){
  let username = userInput.value;
  let password = passInput.value;
  if (username.length > 0 && password.length > 0){
    let url = "http://localhost:8080/?deleteaccount&" + username + "&" + password;
    sendAction(url);
  }
}
