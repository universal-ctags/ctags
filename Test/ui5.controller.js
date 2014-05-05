sap.ui.controller("app.my_form", {

    onInit : function () {
        this.selectListView = null;
        sap.ui.getCore().byId("id_createButton").setEnabled(true);
    },

    refreshForm : function (AUFNR) {
        if (AUFNR && AUFNR !== '') {
            this.objId = this.oView.sId;
        }
        return;
    },

    refreshSettlements : function (AUFNR) {
    },

    setRefreshed : function (value) {
        this.refreshed = value;
    },
});


