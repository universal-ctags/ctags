sap.ui.controller("app.my_form", {

    successfulRequest: function(data) {
        switch( data.mParameters.headers.SAAP_SERVICE ) {
            case SAAP_SERVICE.APPROVAL_DETAIL:
                if (thisForm.getController().mApproval) {
                }
        }
    },

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


