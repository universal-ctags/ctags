/* https://gitlab.gnome.org/GNOME/glib/-/blob/main/gio/gdummyproxyresolver.c */
G_DEFINE_TYPE_WITH_CODE (GDummyProxyResolver, g_dummy_proxy_resolver, G_TYPE_OBJECT,
			 G_IMPLEMENT_INTERFACE (G_TYPE_PROXY_RESOLVER,
						g_dummy_proxy_resolver_iface_init)
			 _g_io_modules_ensure_extension_points_registered ();
			 g_io_extension_point_implement (G_PROXY_RESOLVER_EXTENSION_POINT_NAME,
							 g_define_type_id,
							 "dummy",
							 -100))

static void
g_dummy_proxy_resolver_finalize (GObject *object)
{
  /* must chain up */
  G_OBJECT_CLASS (g_dummy_proxy_resolver_parent_class)->finalize (object);
}
