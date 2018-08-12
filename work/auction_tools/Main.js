function onOpen(e) {
  createAddonMenu();
}

function onEdit(e) {

}

function onInstall(e) {
  onOpen(e);
  resetUserSettings()
}

function authorizeUser(){
  try {
    var addonGroup = GroupsApp.getGroupByEmail("bfatools@googlegroups.com"); //this should return an exception if called by a user who isn't part of the group since non-members can't see member's emails
    var currentUser = Session.getActiveUser();
    if (addonGroup.hasUser(currentUser)){
      SpreadsheetApp.getUi().alert('Authorization successful.');
      authorizedAddonMenu();
    }
    else{
      SpreadsheetApp.getUi().alert('Authorization unsuccessful. Contact developer for more information.');
    }
  }
  catch(err) {
    SpreadsheetApp.getUi().alert('Authorization unsuccessful. Contact developer for more information.');
  }
}

function autoPay(p, checked) {
  pay(p, checked);
  getPurchaserReceipt(p, checked);
}
