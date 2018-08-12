"use strict"
addEventListener('load', start);

var userNumberColumns = [0, 4];
var itemNumberColumns = [0, 3];

var currentUser;
var userColour;
var currentItem;
var itemColour;

function start(){
  //select first row of each table by default
  var userTable = document.getElementById("userTable");
  var itemTable = document.getElementById("itemTable");

  if (!currentUser && userTable.childNodes[1].childNodes[2]){
    currentUser = userTable.childNodes[1].childNodes[2];
    userColour = currentUser.style.background;
    currentUser.style.background = "rgb(102,163,255)";
  }
  if (!currentItem && itemTable.childNodes[1].childNodes[2]){
    currentItem = itemTable.childNodes[1].childNodes[2];
    itemColour = currentItem.style.background;
    currentItem.style.background = "rgb(102,163,255)";
  }

  //set onclick listeners on the headers so they can call sorting function
  var headers = document.getElementsByTagName("TH");
  for (var i = 0; i < headers.length; i++){
    headers[i].addEventListener("click", headerClicked);
    function headerClicked(event){
      let header = event.currentTarget; //get header
      let table = header.parentNode.parentNode.parentNode; //get table

      //get column number
      i = 0;
      while( (header = header.previousSibling) != null ) i++;
      let col = (i+1)/2-1;

      //get whether column is numerical
      let number = false;
      let tableId = table.getAttribute("id");
      if (tableId === "userTable"){
        for (i = 0; i < userNumberColumns.length; i++){
          if (userNumberColumns[i] == col) number = true;
        }
      }
      else if (tableId === "itemTable"){
        for (i = 0; i < itemNumberColumns.length; i++){
          if (itemNumberColumns[i] == col) number = true;
        }
      }

      sortTable(col, table, number);
    }
  }

  //set onclick listeners on the rows so they can be selected
  var rows = document.getElementsByTagName("TR");
  for (i = 0; i < rows.length; i++){
    if (rows[i].getAttribute("class") != "header"){
      rows[i].addEventListener("click", rowClicked);
      function rowClicked(event){
        let row = event.currentTarget;
        let tableId = row.parentNode.parentNode.getAttribute("id"); //get table ID
        if (tableId == "userTable"){
          currentUser.style.background = userColour;
          currentUser = row;
          userColour = row.style.background;
        }
        else if (tableId === "itemTable"){
          currentItem.style.background = itemColour;
          currentItem = row;
          itemColour = row.style.background;
        }
        row.style.background = "rgb(102,163,255)";
      }
    }
  }

  document.getElementById("buyButton").addEventListener("click", buyItem);
  document.getElementById("removeItemButton").addEventListener("click", removeItem);
  document.getElementById("addItemButton").addEventListener("click", addItem);
  document.getElementById("addFundsButton").addEventListener("click", addFunds);
  document.getElementById("removeFundsButton").addEventListener("click", removeFunds);

  var fundsInput = document.getElementById("fundsInput");
  var descInput = document.getElementById("descInput");
  var priceInput = document.getElementById("priceInput");
  fundsInput.value = "";
  descInput.value = "";
  priceInput.value = "";
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

function buyItem(){
  if (currentUser && currentItem){
    let userId = currentUser.childNodes[1].innerHTML;
    let itemId = currentItem.childNodes[1].innerHTML;
    sendAction("http://localhost:8080/?buy&" + userId + "&" + itemId);
  }
}

function removeItem(){
  if (currentUser && currentItem){
    let userId = currentUser.childNodes[1].innerHTML;
    let itemId = currentItem.childNodes[1].innerHTML;
    sendAction("http://localhost:8080/?removeitem&" + userId + "&" + itemId);
  }
}

function addItem(){
  if (currentUser){
    let userId = currentUser.childNodes[1].innerHTML;
    let desc = descInput.value;
    let price = priceInput.value;
    sendAction("http://localhost:8080/?additem&" + userId + "&" + desc + "&" + price);
  }
}

function addFunds(){
  if (currentUser){
    let userId = currentUser.childNodes[1].innerHTML;
    let funds = fundsInput.value;
    sendAction("http://localhost:8080/?addfunds&" + userId + "&" + funds);
  }
}

function removeFunds(){
  if (currentUser){
    let userId = currentUser.childNodes[1].innerHTML;
    let funds = fundsInput.value;
    sendAction("http://localhost:8080/?removefunds&" + userId + "&" + funds);
  }
}

function sortTable(col, table, number){
  let rows, switching, i, x, y, shouldSwitch, dir, switchcount = 0;
  switching = true;
  dir = "asc";

  while (switching) {

    switching = false;
    rows = table.getElementsByTagName("TR");

    for (i = 1; i < (rows.length - 1); i++) {
      shouldSwitch = false;
      x = rows[i].getElementsByTagName("TD")[col];
      y = rows[i + 1].getElementsByTagName("TD")[col];

      if (number){
        x = Number(x.innerHTML);
        y = Number(y.innerHTML);
      }
      else{
        x = x.innerHTML.toLowerCase();
        y = y.innerHTML.toLowerCase();
      }

      if (dir == "asc") {
        if (x > y){
          shouldSwitch = true;
          break;
        }
      }
      else if (dir == "desc") {
        if (x < y){
          shouldSwitch = true;
          break;
        }
      }
    }

    if (shouldSwitch) {
      rows[i].parentNode.insertBefore(rows[i + 1], rows[i]);
      switching = true;
      switchcount ++;
    }
    else {
      if (switchcount == 0 && dir == "asc") {
        dir = "desc";
        switching = true;
      }
    }

  }
}
