add_custom_target(1_tag_this)
add_custom_target(B-add_custom_target-tag_this_2 ${BAR})

    add_custom_target ( c_TAG_THIS#
                        but_not_this)

    ADD_CUSTOM_TARGET		(
        d-ALSO-Tag.this
	)

Add_Custom_target(
e.LastlyTagThis "CustomTarget")

add_custom_target(${not_this})
add_custom_target(${ not_this })
add_custom_target(not/this)


add_executable(1_tag_this)
add_executable(B-add_executable-tag_this_2#[[ set(foo) ]]${BAR})

    add_executable ( c_TAG_THIS but_not_this)

    ADD_EXECUTABLE 	   (
        d-ALSO-Tag.this	
    )

Add_Executable(
e.LastlyTagThis "Executable")

add_executable(${not_this})
add_executable(${ not_this })
add_executable(not/this)


add_library(1_tag_this)
add_library(B-add_library-tag_this_2#
            ${BAR})

    add_library ( c_TAG_THIS but_not_this)

    ADD_LIBRARY       (
		d-ALSO-Tag.this
    )

Add_Library(
e.LastlyTagThis "Library")

add_library(${not_this})
add_library(${ not_this })
add_library(not/this)
