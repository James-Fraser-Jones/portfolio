function rawDataImport(){
  var ss = SpreadsheetApp.getActive();
  var auctioneerSheet = ss.getSheetByName("Auction Sheet");
  var rawData = ss.getSheetByName("raw").getRange(1, 1, 613, 7);
  rawData.copyTo(auctioneerSheet.getRange(2, 1, 613, 7), {contentsOnly:true});
}

/*
function testPay(){
  //var allPurchasers = [5,7,25,50,70,78,109,112,145,156,158,165,166,168,189,196,244,269,306,336,383,387,481,512,618,649,676,757,770,810,812,825,845,902,941,951,976,1069,1083,1111,1151,1240,1312,1321,1322,1384,1427,1453,1471,1487,1493,1500,1538,1590,1638,1640,1641,1643];
  //var allVendors = [9,13,15,20,25,32,40,46,58,61,62,63,69,70,76,77,84,96,105,140,142,150,156,162,166,213,229,336,366,383,480,496,510,547,590,618,650,663,666,737,757,982,992,1080,1081,1111,1117,1151,1170,1176,1240,1264,1359,1370,1407,1409,1412,1490,1535,1558,1580,1594,1609,1610,1611,1619,1629,1634,1636,1666];
  //var soldVendors = [9,13,15,20,25,40,46,58,62,63,69,70,77,105,140,142,156,162,166,213,336,366,480,510,547,590,618,663,666,757,982,992,1080,1081,1111,1117,1151,1176,1240,1264,1359,1370,1412,1535,1580,1594,1611,1619,1629,1634,1636];
  //var unsoldVendors = [32,61,76,84,96,150,229,383,496,650,737,1170,1407,1409,1490,1558,1609,1610,1666];
  //var unpaidPurchasers = [7,50,78,112,156,165,168,196,269,336,387,512,649,757,810,825,902,951,1069,1111,1240,1321,1384,1453,1487,1500,1590,1640,1643];
  var paidPurchasers = [5,25,70,109,145,158,166,189,244,306,383,481,618,676,770,812,845,941,976,1083,1151,1312,1322,1427,1471,1493,1538,1638,1641];
  for (var i = 0; i < paidPurchasers.length; i++){
    pay(paidPurchasers[i]);
  }
}

function testGetSettings(){
  var userSettings = PropertiesService.getUserProperties();
  var properties = userSettings.getProperties();

  for (key in properties){
    Logger.log('properties.' + key + ' = ' + properties[key]);
  }
}

function testSetSettings(){
  var userSettings = PropertiesService.getUserProperties();

  var newProperties = {name: 'Bob', region: 'US', language: 'EN'};

  userSettings.setProperties(newProperties, true);
}

function clientLogger(message){
  Logger.log(message);
}

function testGetGroupEmails(){
  var group = GroupsApp.getGroupByEmail("bfatools@googlegroups.com");
  var groupMembers = group.getUsers();
  Logger.log(groupMembers);
}

function testGetUserEmail(){
  var user = Session.getActiveUser();
  Logger.log(user);
}
*/
