function getUserSettings(){
  return PropertiesService.getUserProperties().getProperties();
}

function setUserSettings(newProperties){
  PropertiesService.getUserProperties().setProperties(newProperties, true);
}

function resetUserSettings(){
  var textFields = ['name', 'address', 'telephone', 'website', 'email', 'semail'];
  var checkFields = ['check'];

  var newProperties = new Object();
  for (var i = 0; i < textFields.length; i++){
    newProperties[textFields[i]] = '';
  }
  for (var i = 0; i < checkFields.length; i++){
    newProperties[checkFields[i]] = false;
  }

  setUserSettings(newProperties);
}
