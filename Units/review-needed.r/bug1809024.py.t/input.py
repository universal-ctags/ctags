   def detach (self):

        model = self.view.props.model
        sort_id, sort_order = tree_sortable_get_sort_column_id (model)
        if sort_id >= 0:
            self.app.state.sort_column = self.find_item_class (id = sort_id)
            if sort_order == gtk.SORT_ASCENDING:
                self.app.state.sort_order = "ascending"
            else:
                self.app.state.sort_order = "descending"
