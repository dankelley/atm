#' @importFrom S7 new_class new_object S7_object
sounding <- S7::new_class("sounding",
    package = "atm",
    properties = list(
        data = class_list,
        metadata = class_list
    ),
    validator = function(self) {
        NULL
    }
    # constructor = function(...) {
    #     S7::new_object(S7_object(),
    #         data = list(),
    #         metadata = list()
    #     )
    # }
)
