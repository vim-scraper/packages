" Menu Translations:	Russian
" Maintainer:		Roman A. Korepanov<roman@72.ru>
" Initial Translation:	Polsky 
" Last Change:	27 Dec 2002

" Quit when menu translations have already been done.
if exists("did_menu_trans")
  finish
endif
let did_menu_trans = 1

scriptencoding cp1251

" Help menu
menutrans &Help			&Помощь
menutrans &Overview<Tab><F1>	&Введение<Tab><F1>
menutrans &How-to\ links	&Как\ сделать\ \+\ Ссылки
menutrans &Credits		&Благодарности
menutrans &User\ Manual         &Юзер\ мануал
menutrans Co&pying		&Лицензионное\ соглашение
menutrans &Version		Ве&рсия
menutrans &About		&О\ программе

" File menu
menutrans &File				&Файл
menutrans &Open\.\.\.<Tab>:e		&Открыть\.\.\.<Tab>:e
menutrans Sp&lit-Open\.\.\.<Tab>:sp	От&крыть\ в\ новом-окне\.\.\.<Tab>:sp
menutrans &New<Tab>:enew       		&Новый<Tab>:enew
menutrans &Close<Tab>:close		&Закрыть<Tab>:close
menutrans &Save<Tab>:w			&Сохранить<Tab>:w
menutrans Save\ &As\.\.\.<Tab>:sav	Сохр&анить\ как\.\.\.<Tab>:sav
menutrans Split\ &Diff\ with\.\.\.	&Разбить\ с\ выводом\ di&ff\.\.\.
menutrans Split\ Patched\ &By\.\.\.	Раз&бить\ с\ выводом\ patc&h\.\.\.
menutrans &Print			&Печать
menutrans Sa&ve-Exit<Tab>:wqa		Запись-в&ыход<Tab>:wqa
menutrans E&xit<Tab>:qa			Вы&ход<Tab>:qa

" Edit menu
menutrans &Edit				&Редактор
menutrans &Undo<Tab>u			&Отмена<Tab>u
menutrans &Redo<Tab>^R			&АнтиОтмена<Tab>^R
menutrans Rep&eat<Tab>\.		По&втор<Tab>\.
menutrans Cu&t<Tab>"+x			Выре&зать<Tab>"+x
menutrans &Copy<Tab>"+y			&Копировать<Tab>"+y
menutrans &Paste<Tab>"+P		В&ставить<Tab>"+p
menutrans Put\ &Before<Tab>[p		Поместить\ перед<Tab>[p
menutrans Put\ &After<Tab>]p		Поместить\ после<Tab>]p
menutrans &Select\ all<Tab>ggVG		Выбрать\ все<Tab>ggVG
menutrans &Find\.\.\.			&Искать\.\.\.
menutrans &Find<Tab>/			Искать<Tab>/
menutrans Find\ and\ Rep&lace\.\.\.	Найти\ и\ &заменить\.\.\.
menutrans Find\ and\ Rep&lace<Tab>:%s	Найти\ и\ &заменить<Tab>:%s
menutrans Find\ and\ Rep&lace		Найти\ и\ &заменить
menutrans Find\ and\ Rep&lace<Tab>:s	Найти\ и\ &заменить<Tab>:s
menutrans Options\.\.\.			&Настройка\.\.\.
menutrans Settings\ &Window		&Установки\ окна
menutrans &Global\ Settings		&Глобальные\ установки
menutrans F&ile\ Settings		Установки\ для\ &файла
menutrans Toggle\ Line\ &Numbering<Tab>:set\ nu!		&Нумерация\ строк\ вкл-выкл<Tab>:set\ nu!
menutrans Toggle\ &List\ Mode<Tab>:set\ list!			Режим\ &просмотра\ вкл-выкл<Tab>:set\ list!
menutrans Toggle\ Line\ &Wrap<Tab>:set\ wrap!		&Автоперевод\ строки<Tab>:set\ wrap!
menutrans Toggle\ W&rap\ at\ word<Tab>:set\ lbr!	Перевод\ &строки\ по\ слову\ вкл-выкл<Tab>:set\ lbr!
menutrans Toggle\ &expand-tab<Tab>:set\ et!			&Расширение\ табуляторов\ вкл-выкл<Tab>:set\ et!
menutrans Toggle\ &auto-indent<Tab>:set\ ai!			А&втоотступ\ вкл-выкл<Tab>:set\ ai!
menutrans Toggle\ &C-indenting<Tab>:set\ cin!			С-отступы\ вкл-выкл<Tab>:set\ cin!
menutrans &Shiftwidth				&Ширина\ перехода\ (shift)
menutrans Te&xt\ Width\.\.\.			&Ширина\ текста\.\.\.
menutrans &File\ Format\.\.\.			Формат\ &файла\.\.\.
menutrans Soft\ &Tabstop				Размер\ &табуляции
menutrans C&olor\ Scheme		&Цветовое\ решение
menutrans &Keymap			Раскладка\ клавиат&уры
menutrans None				Нету
menutrans accents			accents
menutrans hebrew			hebrajski
menutrans hebrewp			hebrajski\ p
menutrans russian-jcuken		русский-jcuken
menutrans russian-jcukenwin		русский-jcukenwin

menutrans Toggle\ Pattern\ &Highlight<Tab>:set\ hls!	Переключение\ &подсветки\ выражений<Tab>:set\ hls!

menutrans Toggle\ &Ignore-case<Tab>:set\ ic!	&Игнорировать\ регистр\ вкл-выкл<Tab>:set\ ic!
menutrans Toggle\ &Showmatch<Tab>:set\ sm!		Просмотр\ &отличий\ вкл-выкл<Tab>:set\ sm!

menutrans &Context\ lines	Контекстные\ &строки
menutrans &Virtual\ Edit	Виртуальное\ &редактирование 
menutrans Never			&Никогда
menutrans Block\ Selection	&Блок\ выборки
menutrans Insert\ mode		Ре&жим\ вставки
menutrans Block\ and\ Insert	Б&лок\ и\ вставка
menutrans Always		&Всегда

menutrans Toggle\ Insert\ &Mode<Tab>:set\ im!	Режим\ в&ставки\ вкл-выкл<Tab>:set\ im!
menutrans Search\ &Path\.\.\.	Найти\ пут&ь\.\.\.
menutrans Ta&g\ Files\.\.\.	Файл\ т&эгов\.\.\.


"
" GUI options
menutrans Toggle\ &Toolbar		Включить\ строку\ статуса
menutrans Toggle\ &Bottom\ Scrollbar	Включить\ нижнюю\ прокурутку
menutrans Toggle\ &Left\ Scrollbar	Включить\ левую\ прокурутку
menutrans Toggle\ &Right\ Scrollbar	Включить\ правую\ прокурутку

" Programming menu
menutrans &Tools			У&тилиты
menutrans &Jump\ to\ this\ tag<Tab>g^]	Прыгнуть\ на\ &этот\ тэг<Tab>g^]
menutrans Jump\ &back<Tab>^T		Прыгнуть\ &обратно<Tab>^T
menutrans Build\ &Tags\ File		Построить\ &файл\ тэгов
" Folding
menutrans &Folding				&Брошюрование\ (отступ)
menutrans &Enable/Disable\ folds<Tab>zi		&Вкл-выкл\ брошюрования<Tab>
menutrans &View\ Cursor\ Line<Tab>zv			&Просмотр\ строки\ курсора<Tab>zv
menutrans Vie&w\ Cursor\ Line\ only<Tab>zMzx			Просмотр\ &только\ строки\ курсора<Tab>zMzx
menutrans C&lose\ more\ folds<Tab>zm				Закрыть\ остальные\ брошюрки<Tab>zm
menutrans &Close\ all\ folds<Tab>zM				&Закрыть\ все\ брошюрки<Tab>zM
menutrans &Open\ all\ folds<Tab>zR				Открыть\ все\ брошюрки<Tab>zR
menutrans O&pen\ more\ folds<Tab>zr				Открыть\ другие\ брошюрки<Tab>zr

menutrans Create\ &Fold<Tab>zf				&Создать\ брошюрку<Tab>zf
menutrans &Delete\ Fold<Tab>zd				&Удалить\ брошюрку<Tab>zd
menutrans Delete\ &All\ Folds<Tab>zD				Удалить\ все\ брошюрки<Tab>zD
menutrans Fold\ column\ &width				&Ширина\ колонки\ брошюрки
menutrans Fold\ Met&hod		&Метод\ брошюрования
menutrans M&anual			П&рочти\ меня
menutrans I&ndent			&Отступ
menutrans E&xpression 			&Выражение
menutrans S&yntax			&Синтаксис
menutrans Ma&rker			&Метка

" Diff
menutrans &Update			&Обновить
menutrans &Get\ Block			&Получить \блок
menutrans &Put\ Block			&Вставить\ блок

" Make and stuff...
menutrans &Make<Tab>:make		В&ыполнить\ make<Tab>:make
menutrans &List\ Errors<Tab>:cl		&Список\ ошибок<Tab>:cl
menutrans L&ist\ Messages<Tab>:cl!	С&писок\ сообщений<Tab>:cl!
menutrans &Next\ Error<Tab>:cn		С&ледующая\ ошибка<Tab>:cn
menutrans &Previous\ Error<Tab>:cp	П&редыдущая\ ошибка<Tab>:cp
menutrans &Older\ List<Tab>:cold	Ст&арый\ список<Tab>:cold
menutrans N&ewer\ List<Tab>:cnew	&Новый\ список<Tab>:cnew
menutrans Error\ &Window  		Окно\ &ошибок
menutrans &Update<Tab>:cwin			О&бновить<Tab>:cwin
menutrans &Close<Tab>:cclose			За&крыть<Tab>:cclose
menutrans &Open<Tab>:copen			Открыт&ь<Tab>:copen

menutrans &Set\ Compiler				&Установка\ компилятора
menutrans &Convert\ to\ HEX<Tab>:%!xxd     Конвертировать\ в\ &16-ричный\ код<Tab>:%!xxd
menutrans Conve&rt\ back<Tab>:%!xxd\ -r     Конвертировать\ &обратно<Tab>:%!xxd\ -r

" Names for buffer menu.
menutrans &Buffers	Бу&феры
menutrans &Refresh\ menu	&Обновить\ меню
menutrans Delete	&Удалить
menutrans &Alternate	&Другое
menutrans &Next		&Следующий
menutrans &Previous	&Предыдущий
menutrans [No\ File]	[Нет\ файла]

" Window menu
menutrans &Window			&Окно
menutrans &New<Tab>^Wn			&Новое<Tab>^Wn
menutrans S&plit<Tab>^Ws		&Разбить<Tab>^Ws
menutrans Sp&lit\ To\ #<Tab>^W^^	Р&азбить на #<Tab>^W^^
menutrans Split\ &Vertically<Tab>^Wv	Ра&збить\ по\ вертикали<Tab>^Wv
menutrans Split\ File\ E&xplorer		Разбить\ &проводник\ файлов
menutrans &Close<Tab>^Wc		&Закрыть<Tab>^Wc
menutrans Close\ &Other(s)<Tab>^Wo	Закр&ыть\ другие<Tab>^Wo
menutrans Ne&xt<Tab>^Ww			&Следующее<Tab>^Ww
menutrans P&revious<Tab>^WW		Предыду&щее<Tab>^WW
menutrans &Equal\ Size<Tab>^W=		&Одинаковый\ размер<Tab>^W=
menutrans &Max\ Height<Tab>^W_		Максимально\ по\ высоте<Tab>^W_
menutrans M&in\ Height<Tab>^W1_		Минимальной\ высоты<Tab>^W1_
menutrans Max\ Width<Tab>^W\|		Максимальная\ ширина<Tab>^W\|
menutrans Min\ Width<Tab>^W1\|		Минимальная\ ширина<Tab>^W1\|
menutrans Max\ &Width<Tab>^W\|		Максимальная\ ширина<Tab>^W\|
menutrans Min\ Widt&h<Tab>^W1\|		Минимальная\ ширина<Tab>^W1\|
menutrans Move\ &To			Переместить\ в
menutrans &Top<Tab>^WK			&Верх<Tab>^WK
menutrans &Bottom<Tab>^WJ		&Низ<Tab>^WJ
menutrans &Left\ side<Tab>^WH		&Левая\ сторона<Tab>^WH
menutrans &Right\ side<Tab>^WL		&Правая\ сторона<Tab>^WL
menutrans Rotate\ &Up<Tab>^WR		&Повернуть\ вверх<Tab>^WR
menutrans Rotate\ &Down<Tab>^Wr		П&овернуть\ вниз<Tab>^Wr
menutrans Split\ &Vertically<Tab>^Wv	Р&азбить\ вертикально<Tab>^Wv
menutrans Select\ Fo&nt\.\.\.		Выбрать\ &шрифт\.\.\.

" The popup menu
menutrans &Undo			&Отмена
menutrans Cu&t			В&ырезать
menutrans &Copy			&Копировать
menutrans &Paste		&Вставить
menutrans &Delete		&Удалить
menutrans Select\ Blockwise	Вы&брать\ блок
menutrans Select\ &Word		Выб&рать\ слово
menutrans Select\ &Line		Выбра&ть\ строку
menutrans Select\ &Block	Выбрат&ь\ блок
menutrans Select\ &All		Выбрать\ вс&е

" The GUI toolbar
if has("toolbar")
  if exists("*Do_toolbar_tmenu")
    delfun Do_toolbar_tmenu
  endif
  fun Do_toolbar_tmenu()
    tmenu ToolBar.Open		Открыть
    tmenu ToolBar.Save		Сохранить
    tmenu ToolBar.SaveAll	Сохранить все
    tmenu ToolBar.Print		Печать
    tmenu ToolBar.Undo		Отмена
    tmenu ToolBar.Redo		Антиотмена
    tmenu ToolBar.Cut		Вырезать
    tmenu ToolBar.Copy		Копировать
    tmenu ToolBar.Paste		Вставить
    tmenu ToolBar.Find		Найти...
    tmenu ToolBar.FindNext	Найти следующий
    tmenu ToolBar.FindPrev	Найти предыдущий
    tmenu ToolBar.Replace	Найти и заменить...
    if 0	" disabled; These are in the Windows menu
      tmenu ToolBar.New		Новое окно
      tmenu ToolBar.WinSplit	Разбить окно
      tmenu ToolBar.WinMax	Развернуть окно
      tmenu ToolBar.WinMin	Свернуть окно
      tmenu ToolBar.WinClose	Закрыть окно
    endif
    tmenu ToolBar.LoadSesn	Загрузить сессию
    tmenu ToolBar.SaveSesn	Сохранить сессию
    tmenu ToolBar.RunScript	Запусть ВИМовский скрипт
    tmenu ToolBar.Make		Выполнить текущий проект (make)
    tmenu ToolBar.Shell		Командная оболочка (Shell)
    tmenu ToolBar.RunCtags	Запустить тэги в текущем каталоге
    tmenu ToolBar.TagJump	Прыгнуть к тэгу под курсором
    tmenu ToolBar.Help		Помощь ВИМа
    tmenu ToolBar.FindHelp	Найти в помощи ВИМа
  endfun
endif

" Syntax menu
menutrans &Syntax 		&Синтаксис
menutrans Set\ '&syntax'\ only	&Установить\ только\ '&Syntax'
menutrans Set\ '&filetype'\ too	У&становить\ также\ '&filetype'
menutrans &Off			&Отключить
menutrans &Manual		&Руководство
menutrans A&utomatic		&Автоматизация
menutrans on/off\ for\ &This\ file			&Вкл-выкл\ для\ этого\ файла
menutrans Co&lor\ test		&Тест\ цветов
menutrans &Highlight\ test	Т&ест\ подсветки
menutrans &Convert\ to\ HTML	&Преобразовать\ в\ HTML

" dialog texts
let menutrans_no_file = "[Нет\ файла]"
let menutrans_help_dialog = "Наберите здесь любое слово, по котрому нужна помощь:\n\nПриставка i_ предшествует названию клавиши (напр. i_CTRL-X)\nПриставка c_ предшествует названию команды  (напр. c_<Del>)\nПриставка ' предшествет названиям опций (напр. 'shiftwidth')"
let g:menutrans_path_dialog = "Введи путь поиска для файлов.\nМожно несколько, через запятую."
let g:menutrans_tags_dialog = "Назови как-то файл тэгов.\nМожно несколько, через запятую."
let g:menutrans_textwidth_dialog = "Введи ширину текста в символах (0 - бесконечно): "
let g:menutrans_fileformat_dialog = "Выбери формат ,в котором этот файл может быть записан"
