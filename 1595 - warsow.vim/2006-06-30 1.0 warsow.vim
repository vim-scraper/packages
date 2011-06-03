" Vim syntax file
" Language:     War§oW 1.0
" Maintainer:   Tomasz 'SpOOnman' Kalkosiñski <tomasz2k@poczta.onet.pl>
" URL:		http://ppp.ctf.pl
" Last change:  29 Jun 2006

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif


syn case ignore

"Logical operators
syn match wswOperator "[-+=&|*!.,~?:/<>]"

"Comments
syn region wswComment	start="\/\/" end="$" oneline contains=wswTodo
syn region wswComment	start="\/\*" end="\*\/" fold contains=wswTodo

"Numbers
syn match wswNumber "[0-9]\+" 

"Strings
syn region wswString start="\"" end="\"" end="$" contains=wswCarret,wswCommand,wswVariable,wswNumber,wswKeyword

"War§oW commands
syn keyword wswCommand	alias aliasa aliaslist attack back bind bindlist callvote camswitch
syn keyword wswCommand	centerview centerviewonvec chase chasenext chaseprev cinematic
syn keyword wswCommand	clear cmd cmdlist condump connect cvarlist demo demoavi demopause
syn keyword wswCommand	devmap disconnect download drop dumpuser echo enterqueue envshot
syn keyword wswCommand	error exec forward fs_dir fs_pakfile fs_path gamemap getinfo
syn keyword wswCommand	gfxinfo give god heartbeat help_hud imagelist in_restart invdrop
syn keyword wswCommand	inven invite invnext invnextw invprev invprevw invuse join kick
syn keyword wswCommand	kill killserver klook leavequeue left load lock lookdown lookup
syn keyword wswCommand	map memlist memstats menu_demos menu_failed menu_game menu_gfx
syn keyword wswCommand	menu_glext menu_joinserver menu_keys menu_main menu_mods menu_msgbox
syn keyword wswCommand	menu_options menu_playerconfig menu_quit menu_setup menu_sound
syn keyword wswCommand	menu_startserver menu_teamconfig menu_video menu_vsays messagemode
syn keyword wswCommand	messagemode2 mlook modellist movedown moveleft moveright moveup
syn keyword wswCommand	music next noclip notready pingserver play players position putaway
syn keyword wswCommand	quit rcon ready reconnect record requestservers right save say
syn keyword wswCommand	say_team score screenshot serverinfo set seta sets setu shaderlist
syn keyword wswCommand	showip showplinks sizedown sizeup snd_restart soundinfo soundlist
syn keyword wswCommand	spec special speed stats status stop stopmusic stopsound strafe
syn keyword wswCommand	sv svuse timein timeout toggle toggleconsole unalias unaliasall
syn keyword wswCommand	unbind unbindall unlock unready use userinfo vid_restart vote vsay
syn keyword wswCommand	vsay_team vstr wait wave weapnext weapprev writeconfig

"Commands special
syn keyword wswCommand	yes no 

"War§oW variables
syn keyword wswVariable	cg_autoaction cg_bloodTrail cg_bloodTrailAlpha cg_bobPitch
syn keyword wswVariable	cg_bobRoll cg_bobSpeed cg_bobYaw cg_centerTime cg_clientHUD
syn keyword wswVariable	cg_crosshair cg_crosshair_color cg_crosshair_size cg_crosshair_strong
syn keyword wswVariable	cg_crosshair_strong_color cg_crosshair_strong_size cg_damage_blend
syn keyword wswVariable	cg_damage_kick cg_debug_HUD cg_debugLoading cg_debugPlayerModels
syn keyword wswVariable	cg_debugWeaponModels cg_decals cg_demo_truePOV cg_draw2D
syn keyword wswVariable	cg_ejectBrass cg_explosionsAlpha cg_explosionsRingAlpha
syn keyword wswVariable	cg_footSteps cg_forceEnemyTeam cg_forceMyTeam cg_gibs
syn keyword wswVariable	cg_grenadeTrail cg_grenadeTrailAlpha cg_gun cg_gunx cg_guny
syn keyword wswVariable	cg_gunz cg_handOffset cg_laserBeamSubdivisions cg_noSkins
syn keyword wswVariable	cg_outlineItemsBlack cg_outlineModels cg_outlinePlayersBlack
syn keyword wswVariable	cg_particles cg_pickup_flash cg_playerTrailsColor cg_predict
syn keyword wswVariable	cg_predictLaserBeam cg_rocketTrail cg_rocketTrailAlpha
syn keyword wswVariable	cg_scoreboardFont cg_scoreboardStats cg_scoreboardWidthScale
syn keyword wswVariable	cg_shadows cg_showBloodTrail cg_showFPS cg_showhelp
syn keyword wswVariable	cg_showHUD cg_showMiss cg_showObituaries cg_showPlayerNames
syn keyword wswVariable	cg_showPlayerNames_alpha cg_showPlayerNames_xoffset
syn keyword wswVariable	cg_showPlayerNames_yoffset cg_showPlayerNames_zfar
syn keyword wswVariable	cg_showPlayerTrails cg_showPointedPlayer cg_showSpeedMeter
syn keyword wswVariable	cg_showTeamLocations cg_showTimer cg_showWeaponSelect cg_simpleItems
syn keyword wswVariable	cg_simpleItemsSize cg_teamBLUEcolor cg_teamBLUEmodel
syn keyword wswVariable	cg_teamBLUEskin cg_teamGREENcolor cg_teamGREENmodel
syn keyword wswVariable	cg_teamGREENskin cg_teamPLAYERScolor cg_teamPLAYERSmodel
syn keyword wswVariable	cg_teamPLAYERSskin cg_teamREDcolor cg_teamREDmodel cg_teamREDskin
syn keyword wswVariable	cg_teamYELLOWcolor cg_teamYELLOWmodel cg_teamYELLOWskin
syn keyword wswVariable	cg_thirdPerson cg_thirdPersonAngle cg_thirdPersonRange
syn keyword wswVariable	cg_viewSize cg_voiceChats cg_volume_announcer cg_volume_effects
syn keyword wswVariable	cg_volume_hitsound cg_volume_players cg_volume_voicechats
syn keyword wswVariable	cg_vwep cg_weaponFlashes cg_weaponlist cl_anglespeedkey
syn keyword wswVariable	cl_battleye cl_compresspackets cl_debug_serverCmd cl_demoavi_fps
syn keyword wswVariable	cl_demoavi_scissor cl_downloads cl_downloads_from_web cl_forwardspeed
syn keyword wswVariable	cl_freelook cl_maxfps cl_maxpackets cl_nodelta cl_pitchspeed
syn keyword wswVariable	cl_run cl_shownet cl_sidespeed cl_stereo cl_stereo_separation
syn keyword wswVariable	cl_synchusercmd cl_timeout cl_upspeed cl_yawspeed cm_noAreas
syn keyword wswVariable	cm_noCurves color com_showtrace con_drawNotify con_fontSystemBig
syn keyword wswVariable	con_fontSystemMedium con_fontSystemSmall con_notifytime
syn keyword wswVariable	con_printText debuggraph dedicated developer developerMemory
syn keyword wswVariable	fixedtime fov fs_basedir fs_basepath fs_cdpath fs_game fs_gamedir
syn keyword wswVariable	fs_usehomedir g_allow_falldamage g_challengers_queue g_countdown_time
syn keyword wswVariable	g_disable_vote_allow_falldamage g_disable_vote_allow_teamdamage
syn keyword wswVariable	g_disable_vote_allready g_disable_vote_challengers_queue
syn keyword wswVariable	g_disable_vote_extended_time g_disable_vote_gametype
syn keyword wswVariable	g_disable_vote_kick g_disable_vote_lock_teams g_disable_vote_map
syn keyword wswVariable	g_disable_vote_maxteamplayers g_disable_vote_maxteams
syn keyword wswVariable	g_disable_vote_maxtimeouts g_disable_vote_mute g_disable_vote_nextmap
syn keyword wswVariable	g_disable_vote_numbots g_disable_vote_remove g_disable_vote_restart
syn keyword wswVariable	g_disable_vote_scorelimit g_disable_vote_timein
syn keyword wswVariable	g_disable_vote_timelimit g_disable_vote_timeout g_disable_vote_vmute
syn keyword wswVariable	g_disable_vote_warmup g_disable_vote_warmup_timelimit
syn keyword wswVariable	g_gametype g_grenade_backoff g_grenade_gravity
syn keyword wswVariable	g_grenade_relative g_instagib gl_cull gl_delayfinish
syn keyword wswVariable	gl_drawbuffer gl_driver gl_ext_bgra gl_ext_compiled_vertex_array
syn keyword wswVariable	gl_ext_compressed_textures gl_ext_draw_range_elements gl_extensions
syn keyword wswVariable	gl_ext_max_texture_filter_anisotropic gl_ext_multitexture
syn keyword wswVariable	gl_ext_NV_texture_env_combine4 gl_ext_texture3D
syn keyword wswVariable	gl_ext_texture_cube_map gl_ext_texture_edge_clamp
syn keyword wswVariable	gl_ext_texture_env_add gl_ext_texture_env_combine
syn keyword wswVariable	gl_ext_texture_env_dot3 gl_ext_texture_filter_anisotropic
syn keyword wswVariable	gl_ext_vertex_buffer_object gl_finish g_maplist g_maprotation
syn keyword wswVariable	g_match_extendedtime g_maxteams g_maxtimeouts g_numbots
syn keyword wswVariable	graphheight graphscale graphshift g_scorelimit g_tctf
syn keyword wswVariable	g_teams_lock g_teams_maxplayers g_teams_teamdamage g_timelimit
syn keyword wswVariable	g_votable_gametypes g_vote_allowed g_vote_electtime g_vote_percent
syn keyword wswVariable	g_warmup_enabled g_warmup_timelimit hand host_speeds in_dgamouse
syn keyword wswVariable	in_grabinconsole in_minmsecs ip logconsole logconsole_append
syn keyword wswVariable	logconsole_flush log_stats lookspring lookstrafe m_accel
syn keyword wswVariable	masterservers m_filter m_forward model m_pitch m_side m_yaw
syn keyword wswVariable	name netgraph net_showfragments nostdout password port protocol
syn keyword wswVariable	qport r_3dlabs_broken r_allow_software r_ambientscale rate r_bloom
syn keyword wswVariable	r_bloom_alpha r_bloom_darken r_bloom_diamond_size r_bloom_fast_sample
syn keyword wswVariable	r_bloom_intensity r_bloom_sample_size r_bumpscale r_clear r_colorbits
syn keyword wswVariable	rcon_address rcon_password r_detailtextures r_directedscale
syn keyword wswVariable	r_drawentities r_draworder r_drawworld r_dynamiclight r_faceplanecull
syn keyword wswVariable	r_fastsky r_flarefade r_flares r_flaresize r_fullbright r_gamma
syn keyword wswVariable	r_ignorehwgamma r_lerpmodels r_lightmap r_lockpvs r_lodbias
syn keyword wswVariable	r_lodscale r_mapoverbrightbits r_maxLMBlockSize r_mode r_nobind
syn keyword wswVariable	r_nocull r_norefresh r_novis r_overbrightbits r_packlightmaps
syn keyword wswVariable	r_picmip r_polyblend r_screenshot_jpeg r_screenshot_jpeg_quality
syn keyword wswVariable	r_shadows_alpha r_shadows_nudge r_shadows_projection_distance
syn keyword wswVariable	r_shownormals r_showtris r_skymip r_speeds r_spherecull r_stencilbits
syn keyword wswVariable	r_subdivisions r_swapinterval r_texturebits r_texturemode s_bits
syn keyword wswVariable	s_channels scr_conspeed s_device sensitivity showdrop showpackets
syn keyword wswVariable	s_initsound s_khz skin s_mixahead s_musicvolume s_show s_swapstereo
syn keyword wswVariable	s_testsound sv_battleye sv_cheats sv_debug_serverCmd sv_enforcetime
syn keyword wswVariable	sv_hostname sv_maxclients sv_maxrate sv_noreload s_volume s_vorbis
syn keyword wswVariable	sv_pps sv_public sv_reconnectlimit sv_showclamp sv_skilllevel
syn keyword wswVariable	sv_timeout sv_uploads sv_uploads_baseurl sv_uploads_from_server
syn keyword wswVariable	sv_zombietime timedemo timegraph timescale ui_filter_battleye
syn keyword wswVariable	ui_filter_empty ui_filter_full ui_filter_gametype ui_filter_password
syn keyword wswVariable	ui_filter_ping ui_filter_skill version vid_fullscreen vid_xpos
syn keyword wswVariable	vid_ypos


"Some War§ow keywords used in player configs
syn keyword wswKeyword	LaserGun Gunblade RiotGun Grenade Rocket Launcher Plasmagun Electrobolt	
syn keyword wswKeyword	weapon flag ammo

"Color names, like ^7Sp^4OO^7nman
syn region wswCarret	matchgroup=wswKeyword start="\^[0-9A-Za-z]" end="[$\^\"\n]\@=" "start : hs=e+1
hi wswCarret		gui=Underline


"War§oW HUD scripting

"PreProc - include (like C)
syn keyword wswPreproc	include

"Conditional commands
syn keyword wswConditional	if else endif

"HUD functions
syn keyword wswFunction setCursor setAlign setSize setFont setFontstyle setColor setFlashColor
syn keyword wswFunction setColorToTeamColor setFlashColorToTeamColor setColorAlpha
syn keyword wswFunction setFlashColorAlpha setRotationSpeed drawFPS drawSpeed drawClock
syn keyword wswFunction drawHelpString drawPlayername drawPointing drawStatString
syn keyword wswFunction drawItemName drawString drawStringNum drawNum drawStretchNum
syn keyword wswFunction drawBar drawPicByIndex drawPicByItemindex drawPicByItemname
syn keyword wswFunction drawPicByName drawModelByIndex drawModelByName drawModelByItemindex
syn keyword wswFunction drawModelByItemname drawWeaponList drawTeamInfo drawRaceTimer
syn keyword wswFunction drawClockText

"HUD constants
syn match wswConstant	"#"
syn keyword wswConstant NOTSET TEAM_SPECTATOR TEAM_PLAYERS TEAM_RED TEAM_BLUE TEAM_GREEN
syn keyword wswConstant TEAM_YELLOW LEFT CENTER RIGHT TOP MIDDLE BOTTOM WIDTH HEIGHT
syn keyword wswConstant GAMETYPE_CTF GAMETYPE_DM GAMETYPE_DUEL GAMETYPE_INSTAGIB
syn keyword wswConstant GAMETYPE_TDM GAMETYPE_MIDAIR GAMETYPE_RACE CTF_STAT_BLUE_FLAG
syn keyword wswConstant FLAG_SAFE FLAG_STOLEN FLAG_DROPPED
syn keyword wswConstant con_fontSystemSmall con_fontSystemMedium con_fontSystemBig

"HUD stats
syn match wswStat "%"
syn keyword wswStat STAT_GAMETYPE STAT_HEALTH STAT_AMMO STAT_AMMO_ITEM STAT_WEAK_AMMO
syn keyword wswStat STAT_ARMOR STAT_ARMOR_ITEM STAT_SELECTED_ITEM STAT_PICKUP_ITEM
syn keyword wswStat STAT_WEAPON_ITEM STAT_POWERUP_ITEM STAT_LAYOUTS STAT_FRAGS
syn keyword wswStat STAT_TEAM STAT_CHASING STAT_POINTED_PLAYER STAT_POINTED_TEAMPLAYER
syn keyword wswStat STAT_RACE_STARTED STAT_RACE_TIME STAT_RACE_PLAYERBESTTIME
syn keyword wswStat STAT_RACE_MATCHBESTTIME STAT_TEAM_RED_SCORE STAT_TEAM_BLUE_SCORE
syn keyword wswStat STAT_TEAM_GREEN_SCORE STAT_TEAM_YELLOW_SCORE STAT_CTF_RED_FLAG
syn keyword wswStat STAT_CTF_BLUE_FLAG

"TODO
syn keyword wswTodo	TODO

syn case match

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_blank_syntax_inits")
  if version < 508
    let did_blank_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink wswPreproc		PreProc
  HiLink wswConditional		Conditional
  HiLink wswComment		Comment
  HiLink wswOperator		Operator
  HiLink wswString		String
  HiLink wswNumber		Number
  HiLink wswStat		Type
  HiLink wswConstant		Define
  HiLink wswFunction		Function
  HiLink wswCommand		Function
  HiLink wswVariable		Identifier
  HiLink wswKeyword		Special
  HiLink wswTodo		Todo

  delcommand HiLink
endif

let b:current_syntax = "warsow"
