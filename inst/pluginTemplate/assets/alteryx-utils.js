/* Utility Functions */

/* DataItems and Display Rules */
function createUIObject(x) {
  function a2ui(d) {
    return { uiobject: d, dataname: d };
  }
  function o2ui(d) {
    return { uiobject: x[d], dataname: d };
  }
  const f = (x.constructor === Array) ? a2ui : o2ui;
  const y = (x.constructor === Array) ? x : Object.keys(x);
  return y.map(f);
}

function makeDataItem(manager, AlteryxDataItems) {
  return function f(id, props) {
    var type = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 'SimpleString';

    var value = void 0;
    var dtype = type;
    var di = manager.GetDataItem(id);
    var newItem = di || new AlteryxDataItems[dtype]({ id: id, dataname: id });
    if (dtype === 'StringSelector' || dtype === 'MultiStringSelector') {
      var data = createUIObject(props.values);
      newItem.setStringList(data);
      value = props.value ? props.value : data[0].dataname;
    } else {
      value = props.value;
    }
    manager.AddDataItem(newItem);
    if (value) newItem.setValue(value);
    return newItem;
  };
}

function displayTarget(targetId, di, cond) {
  var resize = arguments.length <= 3 || arguments[3] === undefined ? false : arguments[3];

  var condition = void 0;
  if (typeof cond == 'undefined') {
    condition = function condition(v) {
      return v;
    };
  } else if (typeof cond == 'string') {
    condition = function condition(v) {
      return v === cond;
    };
  } else {
    condition = cond;
  }
  var dataItem = Alteryx.Gui.manager.GetDataItemByDataName(di);
  var targetDiv = document.getElementById(targetId);
  function display(v) {
    targetDiv.style.display = condition(v) ? 'block' : 'none';
  }
  dataItem.BindUserDataChanged(display);
  display(dataItem.value);
}

function activateDisplayRules(rules){
  Object.keys(rules).map(function(k){
    var v = rules[k]
    if (typeof(v) === 'string'){
      displayTarget(k, v)
    } else {
      displayTarget(k, v[0], v[1])
    }
  })
}

/* Field Map and Setup Complete */
function getDataTypes(x){
  var numericTypes = [
    'Int16', 'Int32', 'Int64', 'Float', 'Double', 'FixedDecimal', 'Byte'
  ];
  var stringTypes = [
    'String', 'WString', 'V_String', 'V_WString'
  ];
  var res = []
  if (x.indexOf('numeric') >= 0) res = res.concat(numericTypes)
  if (x.indexOf('string') >= 0) res = res.concat(stringTypes)
  return res
}

function makeFieldMap(id, allowedTypes){
  var manager = Alteryx.Gui.manager
  var metaInfo0 = manager.metaInfo.Get(0)
  var fields;
  if (metaInfo0){
    fields = metaInfo0._GetFields()
      .filter(d => getDataTypes(allowedTypes).indexOf(d.strType) >= 0)
      .map(d => d.strName);
  } else {
    fields = [];
  }
  var di = manager.GetDataItem(id);
  di.setStringList(fields.map(function(d){
    return {uiobject: d, dataname: d};
  }), true);
}

function handleSetupComplete(){
  var manager = Alteryx.Gui.manager
  var items = ['Model Name', 'Y Var', 'X Vars']
  var showTick = function(){
    var incompleteItems = items
      .map(function(d){return manager.GetDataItem(d).value})
      .filter(function(d){return d == ""})
      .length
    if (incompleteItems === 0) {
      $("#switch-to-customize").css('visibility', 'visible')
    } else {
      $("#switch-to-customize").css('visibility', 'hidden')
    } 
  }
  showTick()
  items.forEach(function(d){
    manager.GetDataItem(d).BindUserDataChanged(showTick)
  })
}

/* Radio, ToggleBar and Initializing Items */
function initializeDataItems(dataItem, items){
  Object.keys(items).map(function(k){
    var itemType = (typeof(items[k]) === "boolean") 
      ? 'SimpleBool' 
      : 'SimpleString';
    dataItem(k, {value: items[k]}, itemType)
  })
}

function initializeRadioItems(dataItem, items){
  Object.keys(items).forEach(function(d){
    items[d].forEach(function(d2){
      dataItem(d2, {value: d2 === items[d][0]}, 'SimpleBool')
    })
  })    
}

function setupRadioGroups(manager, items){
  var dataItem = makeDataItem(manager);
  items.map(function(id){
    var di = manager.GetDataItemByDataName(id)
    var vals = di.StringList.enums.map(function(d){
      return d.dataName
    })
    vals.forEach(function(d2){
      dataItem(d2, {value: d2 === vals[0]}, 'SimpleBool')
    })
  })
}

function syncRadio(id){
  var manager = Alteryx.Gui.manager;
  var di = manager.GetDataItemByDataName(id)
  var vals = di.StringList.enums.map(function(d){
    return d.dataName
  })
  var setRadioVal = function(v){
    vals.map(function(v_){
      manager.GetDataItemByDataName(v_).setValue(v_ === v)
    })
  }
  setRadioVal(di.getValue())
  di.BindUserDataChanged(function(v){
    setRadioVal(v)
  })
}

function initializeToggleBarItems(dataItem, items){
  Object.keys(items).forEach(function(k){
    setupToggleBarItems(dataItem, k, items[k])
  })
}

function setupToggleBarItems(dataItem, dname, values){
  dataItem(dname, {value: values[0], values: values}, 'StringSelector')
  values.forEach(function(v){
    dataItem(v, {value: false}, 'SimpleBool')
  })
}

function setupToggleBar(dname){
  var manager = Alteryx.Gui.manager;
  var x = manager.GetDataItem(dname)
  syncRadio(dname)
  function setToggle(v){
    //console.log("#id-" + v)
    $('#id-' + dname +  ' .toggletab').removeClass('is-tab-selected')
    $(jq("id-" + v)).addClass('is-tab-selected')
  }
  $('#id-' + dname +  ' .toggletab').click(function(){
    x.setValue($(this).data('page'))
    setToggle(x.getValue())
  })
  setToggle(x.getValue())
}

/* Misc. Functions */
function getDefaultModelName(){
  var manager = Alteryx.Gui.manager;
  return manager.toolName
    .replace(/\s/g, '_')
    .replace("(", "")
    .replace(")", "")
}

function getAnnotation(){
  var manager = Alteryx.Gui.manager
  return manager.GetDataItemByDataName("Model Name").value;
}

function handleToggles(curToggle){
  var myToggle = $("#" + curToggle.value);
  myToggle.addClass('accordion-open');
  myToggle.next().addClass('default');
  $('.dt-accordion').find('.accordion-toggle').click(function(){
    //console.log($(this).attr('id'));
    curToggle.setValue($(this).attr('id'));
    $(this).toggleClass('accordion-open');
    $('.accordion-toggle').not($(this)).removeClass('accordion-open');
    
    //Expand or collapse this panel
    $(this).next().slideToggle('fast');

    //Hide the other panels
    $(".accordion-content").not($(this).next()).slideUp('fast');

  });
}

function jq(myid) {
  return "#" + myid.replace( /(:|\.|\[|\]|,|=)/g, "\\$1" );
}
