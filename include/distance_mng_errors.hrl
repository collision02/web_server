-ifndef(DISTANCE_MNG_ERRORS_HRL).
-define(DISTANCE_MNG_ERRORS_HRL,true).

-define(ERR_DISTANCE_MNG_WRONG_REQ, 1).                 %% Некорректный запрос
-define(ERR_DISTANCE_MNG_INTERNAL_ERROR, 2).            %% Внутренняя ошибка
-define(ERR_DISTANCE_MNG_INVALID_FIELD, 3).             %% В запрос передано некорректное поле
-define(ERR_DISTANCE_MNG_FIELD_MISSED, 4).     %% В запросе отсутствует требуемое поле
-define(ERR_DISTANCE_MNG_UNEXPECTED_FIELD, 5).          %% В запросе присутствует лишнее поле
-define(ERR_DISTANCE_MNG_EXPECTING_LIST, 6).            %% В запросе поле ожидается как список
-define(ERR_DISTANCE_MNG_NOTFOUND, 6).            %% В запросе поле ожидается как список


-endif.
