// (c) by TheUnknownOnes under dwywbdbu license - see http://theunknownones.googlecode.com/svn/ for the license

{
  What you need:
    - libvlc.dll
    - libvlccore.dll
    - plugins (easy way: copy the whole folder from your vlc installation)
    - locale folder
}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

unit uLibVLC;

{$DEFINE AutoFixFloatingPointOverflowError}

interface

uses
  SysUtils, StrUtils, Windows;

type
  Plibvlc_instance_t = type Pointer;

  libvlc_exception_t = record
    b_raised : Integer;
    i_code : Integer;
    psz_message : PAnsiChar;
  end;
  Plibvlc_exception_t = ^libvlc_exception_t;

  libvlc_time_t = Int64;

  libvlc_playlist_item_t = record
    i_id : Integer;
    psz_uri : PAnsiChar;
    psz_name : PAnsiChar;
  end;
  Plibvlc_playlist_item_t = ^libvlc_playlist_item_t;

  Plibvlc_log_t = type Pointer;

  Plibvlc_log_iterator_t = type Pointer;

  libvlc_log_message_t = record
    sizeof_msg : Cardinal;   // sizeof() of message structure, must be filled in by user
    i_severity : Integer;    // 0=INFO, 1=ERR, 2=WARN, 3=DBG
    psz_type,                // module type
    psz_name,                // module name
    psz_header,              // optional header
    psz_message : PAnsiChar; // message
  end;
  Plibvlc_log_message_t = ^libvlc_log_message_t;

  Plibvlc_event_manager_t = type Pointer;

  libvlc_event_type_t = (libvlc_MediaMetaChanged,
                          libvlc_MediaSubItemAdded,
                          libvlc_MediaDurationChanged,
                          libvlc_MediaPreparsedChanged,
                          libvlc_MediaFreed,
                          libvlc_MediaStateChanged,

                          libvlc_MediaPlayerNothingSpecial,
                          libvlc_MediaPlayerOpening,
                          libvlc_MediaPlayerBuffering,
                          libvlc_MediaPlayerPlaying,
                          libvlc_MediaPlayerPaused,
                          libvlc_MediaPlayerStopped,
                          libvlc_MediaPlayerForward,
                          libvlc_MediaPlayerBackward,
                          libvlc_MediaPlayerEndReached,
                          libvlc_MediaPlayerEncounteredError,
                          libvlc_MediaPlayerTimeChanged,
                          libvlc_MediaPlayerPositionChanged,
                          libvlc_MediaPlayerSeekableChanged,
                          libvlc_MediaPlayerPausableChanged,

                          libvlc_MediaListItemAdded,
                          libvlc_MediaListWillAddItem,
                          libvlc_MediaListItemDeleted,
                          libvlc_MediaListWillDeleteItem,

                          libvlc_MediaListViewItemAdded,
                          libvlc_MediaListViewWillAddItem,
                          libvlc_MediaListViewItemDeleted,
                          libvlc_MediaListViewWillDeleteItem,

                          libvlc_MediaListPlayerPlayed,
                          libvlc_MediaListPlayerNextItemSet,
                          libvlc_MediaListPlayerStopped,

                          libvlc_MediaDiscovererStarted,
                          libvlc_MediaDiscovererEnded,

                          libvlc_MediaPlayerTitleChanged,
                          libvlc_MediaPlayerSnapshotTaken);

  Plibvlc_media_t = type Pointer;

  libvlc_meta_t = (libvlc_meta_Title,
                    libvlc_meta_Artist,
                    libvlc_meta_Genre,
                    libvlc_meta_Copyright,
                    libvlc_meta_Album,
                    libvlc_meta_TrackNumber,
                    libvlc_meta_Description,
                    libvlc_meta_Rating,
                    libvlc_meta_Date,
                    libvlc_meta_Setting,
                    libvlc_meta_URL,
                    libvlc_meta_Language,
                    libvlc_meta_NowPlaying,
                    libvlc_meta_Publisher,
                    libvlc_meta_EncodedBy,
                    libvlc_meta_ArtworkURL,
                    libvlc_meta_TrackID);

  libvlc_state_t = (libvlc_NothingSpecial=0,
                    libvlc_Opening,
                    libvlc_Buffering,
                    libvlc_Playing,
                    libvlc_Paused,
                    libvlc_Stopped,
                    libvlc_Ended,
                    libvlc_Error);

  Plibvlc_media_list_t = type Pointer;
  Plibvlc_media_list_view_t = type Pointer;

  Plibvlc_media_library_t = type Pointer;

  Plibvlc_media_player_t = type Pointer;

  Plibvlc_track_description_t = ^libvlc_track_description_t;
  libvlc_track_description_t = record
    i_id : Integer;
    psz_name : PAnsiChar;
    p_next : Plibvlc_track_description_t;
  end;

  Plibvlc_audio_output_t = ^libvlc_audio_output_t;
  libvlc_audio_output_t = record
    psz_name : PAnsiChar;
    pst_description : PAnsiChar;
    p_next : Plibvlc_audio_output_t;
  end;

  libvlc_rectangle_t = record
    top, left,
    bottom, right : Integer;
  end;
  Plibvlc_rectangle_t = ^libvlc_rectangle_t;

  libvlc_audio_output_device_types_t = (libvlc_AudioOutputDevice_Error  = -1,
                                        libvlc_AudioOutputDevice_Mono   =  1,
                                        libvlc_AudioOutputDevice_Stereo =  2,
                                        libvlc_AudioOutputDevice_2F2R   =  4,
                                        libvlc_AudioOutputDevice_3F2R   =  5,
                                        libvlc_AudioOutputDevice_5_1    =  6,
                                        libvlc_AudioOutputDevice_6_1    =  7,
                                        libvlc_AudioOutputDevice_7_1    =  8,
                                        libvlc_AudioOutputDevice_SPDIF  = 10);

  libvlc_audio_output_channel_t = (libvlc_AudioChannel_Error   = -1,
                                    libvlc_AudioChannel_Stereo  =  1,
                                    libvlc_AudioChannel_RStereo =  2,
                                    libvlc_AudioChannel_Left    =  3,
                                    libvlc_AudioChannel_Right   =  4,
                                    libvlc_AudioChannel_Dolbys  =  5);

  Plibvlc_media_list_player_t = type Pointer;

  Tmedia_meta_changed = record
    meta_type : libvlc_meta_t;
  end;

  Tmedia_subitem_added = record
    new_child : Plibvlc_media_t;
  end;

  Tmedia_duration_changed = record
    new_duration : Int64;
  end;

  Tmedia_preparsed_changed = record
    new_status : Integer;
  end;

  Tmedia_freed = record
    md : Plibvlc_media_t;
  end;

  Tmedia_state_changed = record
    new_state : libvlc_state_t;
  end;

  Tmedia_player_position_changed = record
    new_position : Single;
  end;

  Tmedia_player_time_changed = record
    new_time : libvlc_time_t;
  end;

  Tmedia_player_title_changed = record
    new_title : Integer;
  end;

  Tmedia_player_seekable_changed = record
    new_seekable : Int64;
  end;

  Tmedia_player_pausable_changed = record
    new_pausable : Int64;
  end;

  Tmedia_list_item_added = record
    item : Plibvlc_media_t;
    index : Integer;
  end;

  Tmedia_list_will_add_item = record
    item : Plibvlc_media_t;
    index : Integer;
  end;

  Tmedia_list_item_deleted = record
    item : Plibvlc_media_t;
    index : Integer;
  end;

  Tmedia_list_will_delete_item = record
    item : Plibvlc_media_t;
    index : Integer;
  end;

  Tmedia_list_view_item_added = record
    item : Plibvlc_media_t;
    index : Integer;
  end;

  Tmedia_list_view_will_add_item = record
    item : Plibvlc_media_t;
    index : Integer;
  end;

  Tmedia_list_view_item_deleted = record
    item : Plibvlc_media_t;
    index : Integer;
  end;

  Tmedia_list_view_will_delete_item = record
    item : Plibvlc_media_t;
    index : Integer;
  end;

  Tmedia_player_snapshot_taken = record
    psz_filename : PAnsiChar;
  end;

  libvlc_event_t = record
    _type : libvlc_event_type_t;
    p_obj : Pointer;
    case Integer of
      0 : (media_meta_changed : Tmedia_meta_changed);
      1 : (media_subitem_added : Tmedia_subitem_added);
      2 : (media_duration_changed : Tmedia_duration_changed);
      3 : (media_preparsed_changed : Tmedia_preparsed_changed);
      4 : (media_freed : Tmedia_freed);
      5 : (media_state_changed : Tmedia_state_changed);
      6 : (media_player_position_changed : Tmedia_player_position_changed);
      7 : (media_player_time_changed : Tmedia_player_time_changed);
      8 : (media_player_title_changed : Tmedia_player_title_changed);
      9 : (media_player_seekable_changed : Tmedia_player_seekable_changed);
      10: (media_player_pausable_changed : Tmedia_player_pausable_changed);
      11: (media_list_item_added : Tmedia_list_item_added);
      12: (media_list_will_add_item : Tmedia_list_will_add_item);
      13: (media_list_item_deleted : Tmedia_list_item_deleted);
      14: (media_list_will_delete_item : Tmedia_list_will_delete_item);
      15: (media_list_view_item_added : Tmedia_list_view_item_added);
      16: (media_list_view_will_add_item : Tmedia_list_view_will_add_item);
      17: (media_list_view_item_deleted : Tmedia_list_view_item_deleted);
      18: (media_list_view_will_delete_item : Tmedia_list_view_will_delete_item);
      19: (media_player_snapshot_taken : Tmedia_player_snapshot_taken);
  end;

  Plibvlc_event_t = ^libvlc_event_t;

  libvlc_callback_t = procedure(p_event : Plibvlc_event_t; userdata : Pointer); cdecl;

  Plibvlc_media_discoverer_t = type Pointer;

  ILibVLC = interface
    ['{751843F3-0D38-4B19-8863-5D57B6E19A2E}']

    procedure libvlc_exception_init(p_exception : Plibvlc_exception_t);
    function libvlc_exception_raised(p_exception : Plibvlc_exception_t) : Integer;
    procedure libvlc_exception_raise(p_exception : Plibvlc_exception_t;
                                     psz_format : PAnsiChar;
                                     var AData);
    procedure libvlc_exception_clear(p_exception : Plibvlc_exception_t);
    function libvlc_exception_get_message(p_exception : Plibvlc_exception_t) : PAnsiChar;

    function libvlc_new(argc : Integer;
                        argv : PPAnsiChar;
                        p_exception : Plibvlc_exception_t) : Plibvlc_instance_t;
    procedure libvlc_release(p_instance : Plibvlc_instance_t);
    procedure libvlc_retain(p_instance : Plibvlc_instance_t);
    procedure libvlc_add_intf(p_instance : Plibvlc_instance_t;
                              name : PAnsiChar;
                              p_exception : Plibvlc_exception_t);
    procedure libvlc_wait(p_instance : Plibvlc_instance_t);
    function libvlc_get_version() : PAnsiChar;
    function libvlc_get_compiler() : PAnsiChar;
    function libvlc_get_changeset() : PAnsiChar;
    procedure libvlc_free(AData : Pointer);

    procedure libvlc_event_attach(p_event_manager : Plibvlc_event_manager_t;
                                  event_type : libvlc_event_type_t;
                                  f_callback : libvlc_callback_t;
                                  userdata : Pointer;
                                  p_e : Plibvlc_exception_t);
    procedure libvlc_event_detach(p_event_manager : Plibvlc_event_manager_t;
                                  event_type : libvlc_event_type_t;
                                  f_callback : libvlc_callback_t;
                                  userdata : Pointer;
                                  p_e : Plibvlc_exception_t);
    function libvlc_event_type_name(event_type : libvlc_event_type_t) : PAnsiChar;

    function libvlc_get_log_verbosity(p_instance : Plibvlc_instance_t;
                                      p_e : Plibvlc_exception_t) : Cardinal;
    procedure libvlc_set_log_verbosity(p_instance : Plibvlc_instance_t;
                                       level : Cardinal;
                                       p_e : Plibvlc_exception_t);
    function libvlc_log_open(p_instance : Plibvlc_instance_t;
                             p_exception : Plibvlc_exception_t) : Plibvlc_log_t;
    procedure libvlc_log_close(p_log : Plibvlc_log_t;
                               p_exception : Plibvlc_exception_t);
    function libvlc_log_count(p_log : Plibvlc_log_t;
                              p_exception : Plibvlc_exception_t) : Cardinal;
    procedure libvlc_log_clear(p_log : Plibvlc_log_t;
                               p_exception : Plibvlc_exception_t);
    function libvlc_log_get_iterator(p_log : Plibvlc_log_t;
                                     p_exception : Plibvlc_exception_t) : Plibvlc_log_iterator_t;
    procedure libvlc_log_iterator_free(p_iter : Plibvlc_log_iterator_t;
                                       p_exception : Plibvlc_exception_t);
    function libvlc_log_iterator_has_next(p_iter : Plibvlc_log_iterator_t;
                                          p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_log_iterator_next(p_iter : Plibvlc_log_iterator_t;
                                      p_buffer : Plibvlc_log_message_t;
                                      p_exception : Plibvlc_exception_t) : Plibvlc_log_message_t;

    function libvlc_media_new(p_instance : Plibvlc_instance_t;
                               psz_mrl : PAnsiChar;
                               p_exception : Plibvlc_exception_t) : Plibvlc_media_t;
    function libvlc_media_new_as_node(p_instance : Plibvlc_instance_t;
                                      psz_name : PAnsiChar;
                                      p_exception : Plibvlc_exception_t) : Plibvlc_media_t;
    procedure libvlc_media_add_option(p_media : Plibvlc_media_t;
                                      ppsz_options : PAnsiChar;
                                      p_exception : Plibvlc_exception_t);
    procedure libvlc_media_add_option_untrusted(p_media : Plibvlc_media_t;
                                                ppsz_options : PAnsiChar;
                                                p_exception : Plibvlc_exception_t);
    procedure libvlc_media_retain(p_media : Plibvlc_media_t);
    procedure libvlc_media_release(p_media : Plibvlc_media_t);
    function libvlc_media_get_mrl(p_media : Plibvlc_media_t;
                                  p_exception : Plibvlc_exception_t) : PAnsiChar;
    function libvlc_media_duplicate(p_media : Plibvlc_media_t) : Plibvlc_media_t;
    function libvlc_media_get_meta(p_media : Plibvlc_media_t;
                                   e_meta : libvlc_meta_t;
                                   p_exception : Plibvlc_exception_t) : PAnsiChar;
    function libvlc_media_get_state(p_media : Plibvlc_media_t;
                                    p_exception : Plibvlc_exception_t) : libvlc_state_t;
    function libvlc_media_subitems(p_media : Plibvlc_media_t;
                                   p_exception : Plibvlc_exception_t) : Plibvlc_media_list_t;
    function libvlc_media_event_manager(p_media : Plibvlc_media_t;
                                        p_exception : Plibvlc_exception_t) : Plibvlc_event_manager_t;
    function libvlc_media_get_duration(p_media : Plibvlc_media_t;
                                       p_exception : Plibvlc_exception_t) : libvlc_time_t;
    function libvlc_media_is_preparsed(p_media : Plibvlc_media_t;
                                       p_exception : Plibvlc_exception_t) : Integer;
    procedure libvlc_media_set_user_data(p_media : Plibvlc_media_t;
                                         p_new_user_data : Pointer;
                                         p_exception : Plibvlc_exception_t);
    function libvlc_media_get_user_data(p_media : Plibvlc_media_t;
                                        p_exception : Plibvlc_exception_t) : Pointer;

    function libvlc_media_list_new(p_instance : Plibvlc_instance_t;
                                   p_exception : Plibvlc_exception_t) : Plibvlc_media_list_t;
    procedure libvlc_media_list_release(p_media_list : Plibvlc_media_list_t);
    procedure libvlc_media_list_retain(p_media_list : Plibvlc_media_list_t);
    procedure libvlc_media_list_set_media(p_media_list : Plibvlc_media_list_t;
                                          p_media : Plibvlc_media_t;
                                          p_exception : Plibvlc_exception_t);
    function libvlc_media_list_media(p_media_list : Plibvlc_media_list_t;
                                     p_exception : Plibvlc_exception_t) : Plibvlc_media_t;
    procedure libvlc_media_list_add_media(p_media_list : Plibvlc_media_list_t;
                                          p_media : Plibvlc_media_t;
                                          p_exception : Plibvlc_exception_t);
    procedure libvlc_media_list_insert_media(p_media_list : Plibvlc_media_list_t;
                                             p_media : Plibvlc_media_t;
                                             index : Integer;
                                             p_exception : Plibvlc_exception_t);
    procedure libvlc_media_list_remove_index(p_media_list : Plibvlc_media_list_t;
                                             index : Integer;
                                             p_exception : Plibvlc_exception_t);
    function libvlc_media_list_count(p_media_list : Plibvlc_media_list_t;
                                     p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_media_list_item_at_index(p_media_list : Plibvlc_media_list_t;
                                             index : Integer;
                                             p_exception : Plibvlc_exception_t) : Plibvlc_media_t;
    function libvlc_media_list_index_of_item(p_media_list : Plibvlc_media_list_t;
                                             p_media : Plibvlc_media_t;
                                             p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_media_list_is_readonly(p_media_list : Plibvlc_media_list_t) : Integer;
    procedure libvlc_media_list_lock(p_media_list : Plibvlc_media_list_t);
    procedure libvlc_media_list_unlock(p_media_list : Plibvlc_media_list_t);
    function libvlc_media_list_flat_view(p_media_list : Plibvlc_media_list_t;
                                         p_exception : Plibvlc_exception_t) : Plibvlc_media_list_view_t;
    function libvlc_media_list_hierarchical_view(p_media_list : Plibvlc_media_list_t;
                                                 p_exception : Plibvlc_exception_t) : Plibvlc_media_list_view_t;
    function libvlc_media_list_hierarchical_node_view(p_media_list : Plibvlc_media_list_t;
                                                      p_exception : Plibvlc_exception_t) : Plibvlc_media_list_view_t;
    function libvlc_media_list_event_manager(p_media_list : Plibvlc_media_list_t;
                                             p_exception : Plibvlc_exception_t) : Plibvlc_event_manager_t;

    procedure libvlc_media_list_view_retain(p_media_list_view : Plibvlc_media_list_view_t);
    procedure libvlc_media_list_view_release(p_media_list_view : Plibvlc_media_list_view_t);
    function libvlc_media_list_view_event_manager(p_media_list_view : Plibvlc_media_list_view_t) : Plibvlc_event_manager_t;
    function libvlc_media_list_view_count(p_media_list_view : Plibvlc_media_list_view_t;
                                          p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_media_list_view_item_at_index(p_media_list_view : Plibvlc_media_list_view_t;
                                                  i_index : Integer;
                                                  p_exception : Plibvlc_exception_t) : Plibvlc_media_t;
    function libvlc_media_list_view_children_at_index(p_media_list_view : Plibvlc_media_list_view_t;
                                                      index : Integer;
                                                      p_exception : Plibvlc_exception_t) : Plibvlc_media_list_view_t;
    function libvlc_media_list_view_children_for_item(p_media_list_view : Plibvlc_media_list_view_t;
                                                      p_media : Plibvlc_media_t;
                                                      p_exception : Plibvlc_exception_t) : Plibvlc_media_list_view_t;
    function libvlc_media_list_view_index_of_item(p_media_list_view : Plibvlc_media_list_view_t;
                                                  p_media : Plibvlc_media_t;
                                                  p_exception : Plibvlc_exception_t) : Integer;
    procedure libvlc_media_list_view_insert_at_index(p_media_list_view : Plibvlc_media_list_view_t;
                                                     p_media : Plibvlc_media_t;
                                                     index : Integer;
                                                     p_exception : Plibvlc_exception_t);
    procedure libvlc_media_list_view_remove_at_index(p_media_list_view : Plibvlc_media_list_view_t;
                                                     index : Integer;
                                                     p_exception : Plibvlc_exception_t);
    procedure libvlc_media_list_view_add_item(p_media_list_view : Plibvlc_media_list_view_t;
                                              p_media : Plibvlc_media_t;
                                              p_exception : Plibvlc_exception_t);
    function libvlc_media_list_view_parent_media_list(p_media_list_view : Plibvlc_media_list_view_t;
                                                      p_exception : Plibvlc_exception_t) : Plibvlc_media_list_t;

    function libvlc_media_library_new(p_instance : Plibvlc_instance_t;
                                      p_exception : Plibvlc_exception_t) : Plibvlc_media_library_t;
    procedure libvlc_media_library_release(p_mlib : Plibvlc_media_library_t);
    procedure libvlc_media_library_retain(p_mlib : Plibvlc_media_library_t);
    procedure libvlc_media_library_load(p_mlib : Plibvlc_media_library_t;
                                        p_exception : Plibvlc_exception_t);
    procedure libvlc_media_library_save(p_mlib : Plibvlc_media_library_t;
                                        p_exception : Plibvlc_exception_t);
    function libvlc_media_library_media_list(p_mlib : Plibvlc_media_library_t;
                                             p_exception : Plibvlc_exception_t) : Plibvlc_media_list_t;

    function libvlc_media_player_new(p_instance : Plibvlc_instance_t;
                                     p_exception : Plibvlc_exception_t) : Plibvlc_media_player_t;
    function libvlc_media_player_new_from_media(p_media : Plibvlc_media_t;
                                                p_exception : Plibvlc_exception_t) : Plibvlc_media_player_t;
    procedure libvlc_media_player_release(p_media_player : Plibvlc_media_player_t);
    procedure libvlc_media_player_retain(p_media_player : Plibvlc_media_player_t);
    procedure libvlc_media_player_set_media(p_media_player : Plibvlc_media_player_t;
                                            p_media : Plibvlc_media_t;
                                            p_exception : Plibvlc_exception_t);
    function libvlc_media_player_get_media(p_media_player : Plibvlc_media_player_t;
                                           p_exception : Plibvlc_exception_t) : Plibvlc_media_t;
    function libvlc_media_player_event_manager(p_media_player : Plibvlc_media_player_t;
                                               p_exception : Plibvlc_exception_t) : Plibvlc_event_manager_t;
    function libvlc_media_player_is_playing(p_media_player : Plibvlc_media_player_t;
                                            p_exception : Plibvlc_exception_t) : Integer;
    procedure libvlc_media_player_play(p_media_player : Plibvlc_media_player_t;
                                       p_exception : Plibvlc_exception_t);
    procedure libvlc_media_player_pause(p_media_player : Plibvlc_media_player_t;
                                        p_exception : Plibvlc_exception_t);
    procedure libvlc_media_player_stop(p_media_player : Plibvlc_media_player_t;
                                       p_exception : Plibvlc_exception_t);
    procedure libvlc_media_player_set_nsobject(p_media_player : Plibvlc_media_player_t;
                                               drawable : Pointer;
                                               p_exception : Plibvlc_exception_t);
    function libvlc_media_player_get_nsobject(p_media_player : Plibvlc_media_player_t) : Pointer;
    procedure libvlc_media_player_set_agl(p_media_player : Plibvlc_media_player_t;
                                          drawable : Cardinal;
                                          p_exception : Plibvlc_exception_t);
    function libvlc_media_player_get_agl(p_media_player : Plibvlc_media_player_t) : Cardinal;
    procedure libvlc_media_player_set_xwindow(p_media_player : Plibvlc_media_player_t;
                                              drawable : Cardinal;
                                              p_exception : Plibvlc_exception_t);
    function libvlc_media_player_get_xwindow(p_media_player : Plibvlc_media_player_t) : Cardinal;
    procedure libvlc_media_player_set_hwnd(p_media_player : Plibvlc_media_player_t;
                                           drawable : Pointer;
                                           p_exception : Plibvlc_exception_t);
    function libvlc_media_player_get_hwnd(p_media_player : Plibvlc_media_player_t) : Pointer;
    function libvlc_media_player_get_length(p_media_player : Plibvlc_media_player_t;
                                            p_exception : Plibvlc_exception_t) : libvlc_time_t;
    function libvlc_media_player_get_time(p_media_player : Plibvlc_media_player_t;
                                          p_exception : Plibvlc_exception_t) : libvlc_time_t;
    procedure libvlc_media_player_set_time(p_media_player : Plibvlc_media_player_t;
                                           time : libvlc_time_t;
                                           p_exception : Plibvlc_exception_t);
    function libvlc_media_player_get_position(p_media_player : Plibvlc_media_player_t;
                                              p_exception : Plibvlc_exception_t) : Single;
    procedure libvlc_media_player_set_position(p_media_player : Plibvlc_media_player_t;
                                               position : Single;
                                               p_exception : Plibvlc_exception_t);
    procedure libvlc_media_player_set_chapter(p_media_player : Plibvlc_media_player_t;
                                              chapter : Integer;
                                              p_exception : Plibvlc_exception_t);
    function libvlc_media_player_get_chapter(p_media_player : Plibvlc_media_player_t;
                                             p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_media_player_get_chapter_count(p_media_player : Plibvlc_media_player_t;
                                                   p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_media_player_will_play(p_media_player : Plibvlc_media_player_t;
                                           p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_media_player_get_chapter_count_for_title(p_media_player : Plibvlc_media_player_t;
                                                             title : Integer;
                                                             p_exception : Plibvlc_exception_t) : Integer;
    procedure libvlc_media_player_set_title(p_media_player : Plibvlc_media_player_t;
                                            title : Integer;
                                            p_exception : Plibvlc_exception_t);
    function libvlc_media_player_get_title(p_media_player : Plibvlc_media_player_t;
                                           p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_media_player_get_title_count(p_media_player : Plibvlc_media_player_t;
                                                 p_exception : Plibvlc_exception_t) : Integer;
    procedure libvlc_media_player_previous_chapter(p_media_player : Plibvlc_media_player_t;
                                                   p_exception : Plibvlc_exception_t);
    procedure libvlc_media_player_next_chapter(p_media_player : Plibvlc_media_player_t;
                                               p_exception : Plibvlc_exception_t);
    function libvlc_media_player_get_rate(p_media_player : Plibvlc_media_player_t;
                                          p_exception : Plibvlc_exception_t) : Single;
    procedure libvlc_media_player_set_rate(p_media_player : Plibvlc_media_player_t;
                                           rate : Single;
                                           p_exception : Plibvlc_exception_t);
    function libvlc_media_player_get_state(p_media_player : Plibvlc_media_player_t;
                                           p_exception : Plibvlc_exception_t) : libvlc_state_t;
    function libvlc_media_player_get_fps(p_media_player : Plibvlc_media_player_t;
                                         p_exception : Plibvlc_exception_t) : Single;
    function libvlc_media_player_has_vout(p_media_player : Plibvlc_media_player_t;
                                           p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_media_player_is_seekable(p_media_player : Plibvlc_media_player_t;
                                             p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_media_player_can_pause(p_media_player : Plibvlc_media_player_t;
                                           p_exception : Plibvlc_exception_t) : Integer;
    procedure libvlc_track_description_release(p_track_description : Plibvlc_track_description_t);
    procedure libvlc_toggle_fullscreen(p_media_player : Plibvlc_media_player_t;
                                       p_exception : Plibvlc_exception_t);
    procedure libvlc_set_fullscreen(p_media_player : Plibvlc_media_player_t;
                                    enabled : Integer;
                                    p_exception : Plibvlc_exception_t);
    function libvlc_get_fullscreen(p_media_player : Plibvlc_media_player_t;
                                   p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_video_get_height(p_media_player : Plibvlc_media_player_t;
                                     p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_video_get_width(p_media_player : Plibvlc_media_player_t;
                                    p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_video_get_scale(p_media_player : Plibvlc_media_player_t;
                                     p_exception : Plibvlc_exception_t) : Single;
    procedure libvlc_video_set_scale(p_media_player : Plibvlc_media_player_t;
                                     scale : Single;
                                     p_exception : Plibvlc_exception_t);
    function libvlc_video_get_aspect_ratio(p_media_player : Plibvlc_media_player_t;
                                           p_exception : Plibvlc_exception_t) : PAnsiChar;
    procedure libvlc_video_set_aspect_ratio(p_media_player : Plibvlc_media_player_t;
                                            psz_aspect : PAnsiChar;
                                            p_exception : Plibvlc_exception_t);
    function libvlc_video_get_spu(p_media_player : Plibvlc_media_player_t;
                                  p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_video_get_spu_count(p_media_player : Plibvlc_media_player_t;
                                        p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_video_get_spu_description(p_media_player : Plibvlc_media_player_t;
                                              p_exception : Plibvlc_exception_t) : Plibvlc_track_description_t;
    procedure libvlc_video_set_spu(p_media_player : Plibvlc_media_player_t;
                                   i_spu : Integer;
                                   p_exception : Plibvlc_exception_t);
    function libvlc_video_set_subtitle_file(p_media_player : Plibvlc_media_player_t;
                                            filename : PAnsiChar;
                                            p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_video_get_title_description(p_media_player : Plibvlc_media_player_t;
                                                p_exception : Plibvlc_exception_t) : Plibvlc_track_description_t;
    function libvlc_video_get_chapter_description(p_media_player : Plibvlc_media_player_t;
                                                  title : Integer;
                                                  p_exception : Plibvlc_exception_t) : Plibvlc_track_description_t;
    function libvlc_video_get_crop_geometry(p_media_player : Plibvlc_media_player_t;
                                            p_exception : Plibvlc_exception_t) : PAnsiChar;
    procedure libvlc_video_set_crop_geometry(p_media_player : Plibvlc_media_player_t;
                                             geometry : PAnsiChar;
                                             p_exception : Plibvlc_exception_t);
    procedure libvlc_toggle_teletext(p_media_player : Plibvlc_media_player_t;
                                     p_exception : Plibvlc_exception_t);
    function libvlc_video_get_teletext(p_media_player : Plibvlc_media_player_t;
                                       p_exception : Plibvlc_exception_t) : Integer;
    procedure libvlc_video_set_teletext(p_media_player : Plibvlc_media_player_t;
                                        page : Integer;
                                        p_exception : Plibvlc_exception_t);
    function libvlc_video_get_track_count(p_media_player : Plibvlc_media_player_t;
                                          p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_video_get_track_description(p_media_player : Plibvlc_media_player_t;
                                                p_exception : Plibvlc_exception_t) : Plibvlc_track_description_t;
    function libvlc_video_get_track(p_media_player : Plibvlc_media_player_t;
                                    p_exception : Plibvlc_exception_t) : Integer;
    procedure libvlc_video_set_track(p_media_player : Plibvlc_media_player_t;
                                     track : Integer;
                                     p_exception : Plibvlc_exception_t);
    procedure libvlc_video_take_snapshot(p_media_player : Plibvlc_media_player_t;
                                         filepath : PAnsiChar;
                                         width, height : Integer;
                                         p_exception : Plibvlc_exception_t);
    function libvlc_audio_output_list_get(p_instance : Plibvlc_instance_t;
                                          p_exception : Plibvlc_exception_t) : Plibvlc_audio_output_t;
    procedure libvlc_audio_output_list_release(audio_output_list : Plibvlc_audio_output_t);
    function libvlc_audio_output_set(p_instance : Plibvlc_instance_t;
                                     psz_audio_output : PAnsiChar) : Integer;
    function libvlc_audio_output_device_count(p_instance : Plibvlc_instance_t;
                                              psz_audio_output : PAnsiChar) : Integer;
    function libvlc_audio_output_device_longname(p_instance : Plibvlc_instance_t;
                                                 psz_audio_output : PAnsiChar;
                                                 device : Integer) : PAnsiChar;
    function libvlc_audio_output_device_id(p_instance : Plibvlc_instance_t;
                                           psz_audio_output : PAnsiChar;
                                           device : Integer) : PAnsiChar;
    procedure libvlc_audio_output_device_set(p_instance : Plibvlc_instance_t;
                                              psz_audio_output : PAnsiChar;
                                              device : PAnsiChar);
    function libvlc_audio_output_get_device_type(p_instance : Plibvlc_instance_t;
                                                 p_exception : Plibvlc_exception_t) : Integer;
    procedure libvlc_audio_output_set_device_type(p_instance : Plibvlc_instance_t;
                                                  device_type : Integer;
                                                  p_exception : Plibvlc_exception_t);
    procedure libvlc_audio_toggle_mute(p_instance : Plibvlc_instance_t;
                                       p_exception : Plibvlc_exception_t);
    function libvlc_audio_get_mute(p_instance : Plibvlc_instance_t;
                                   p_exception : Plibvlc_exception_t) : Integer;
    procedure libvlc_audio_set_mute(p_instance : Plibvlc_instance_t;
                                    status : Integer;
                                    p_exception : Plibvlc_exception_t);
    function libvlc_audio_get_volume(p_instance : Plibvlc_instance_t;
                                     p_exception : Plibvlc_exception_t) : Integer;
    procedure libvlc_audio_set_volume(p_instance : Plibvlc_instance_t;
                                      volume : Integer;
                                      p_exception : Plibvlc_exception_t);
    function libvlc_audio_get_track_count(p_media_player : Plibvlc_media_player_t;
                                          p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_audio_get_track_description(p_media_player : Plibvlc_media_player_t;
                                                p_exception : Plibvlc_exception_t) : Plibvlc_track_description_t;
    function libvlc_audio_get_track(p_media_player : Plibvlc_media_player_t;
                                    p_exception : Plibvlc_exception_t) : Integer;
    procedure libvlc_audio_set_track(p_media_player : Plibvlc_media_player_t;
                                     p_exception : Plibvlc_exception_t);
    function libvlc_audio_get_channel(p_instance : Plibvlc_instance_t;
                                      p_exception : Plibvlc_exception_t) : Integer;
    procedure libvlc_audio_set_channel(p_instance : Plibvlc_instance_t;
                                       channel : Integer;
                                       p_exception : Plibvlc_exception_t);

    procedure libvlc_media_list_player_play(p_mlp : Plibvlc_media_list_player_t;
                                            p_exception : Plibvlc_exception_t);
    procedure libvlc_media_list_player_pause(p_mlp : Plibvlc_media_list_player_t;
                                             p_exception : Plibvlc_exception_t);
    function libvlc_media_list_player_is_playing(p_mlp : Plibvlc_media_list_player_t;
                                                 p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_media_list_player_get_state(p_mlp : Plibvlc_media_list_player_t;
                                                p_exception : Plibvlc_exception_t) : libvlc_state_t;
    procedure libvlc_media_list_player_play_item_at_index(p_mlp : Plibvlc_media_list_player_t;
                                                          i_index : Integer;
                                                          p_exception : Plibvlc_exception_t);
    procedure libvlc_media_list_player_play_item(p_mlp : Plibvlc_media_list_player_t;
                                                 p_media : Plibvlc_media_t;
                                                 p_exception : Plibvlc_exception_t);
    procedure libvlc_media_list_player_stop(p_mlp : Plibvlc_media_list_player_t;
                                            p_exception : Plibvlc_exception_t);
    procedure libvlc_media_list_player_next(p_mlp : Plibvlc_media_list_player_t;
                                            p_exception : Plibvlc_exception_t);

    function libvlc_media_discoverer_new_from_name(p_instance : Plibvlc_instance_t;
                                                   psz_name : PAnsiChar;
                                                   p_exception : Plibvlc_exception_t) : Plibvlc_media_discoverer_t;
    procedure libvlc_media_discoverer_release(p_mdis : Plibvlc_media_discoverer_t);
    function libvlc_media_discoverer_localized_name(p_mdis : Plibvlc_media_discoverer_t) : PAnsiChar;
    function libvlc_media_discoverer_media_list(p_mdis : Plibvlc_media_discoverer_t) : Plibvlc_media_list_t;
    function libvlc_media_discoverer_event_manager(p_mdis : Plibvlc_media_discoverer_t) : Plibvlc_event_manager_t;
    function libvlc_media_discoverer_is_running(p_mdis : Plibvlc_media_discoverer_t) : Integer;

    procedure libvlc_vlm_release(p_instance : Plibvlc_instance_t;
                                 p_exception : Plibvlc_exception_t);
    procedure libvlc_vlm_add_broadcast(p_instance : Plibvlc_instance_t;
                                       psz_name,
                                       psz_input,
                                       psz_output : PAnsiChar;
                                       options : Integer;
                                       ppsz_options : Pointer;
                                       b_enabled : Integer;
                                       b_loop : Integer;
                                       p_exception : Plibvlc_exception_t);
    procedure libvlc_vlm_add_vod(p_instance : Plibvlc_instance_t;
                                 psz_name,
                                 psz_input : PAnsiChar;
                                 i_options : Integer;
                                 ppsz_options : Pointer;
                                 b_enabled : Integer;
                                 psz_mux : PAnsiChar;
                                 p_exception : Plibvlc_exception_t);

    procedure libvlc_vlm_del_media(p_instance : Plibvlc_instance_t;
                                   psz_name : PAnsiChar;
                                   p_exception : Plibvlc_exception_t);
    procedure libvlc_vlm_set_enabled(p_instance : Plibvlc_instance_t;
                                     psz_name : PAnsiChar;
                                     b_enabled : Integer;
                                     p_exception : Plibvlc_exception_t);
    procedure libvlc_vlm_set_output(p_instance : Plibvlc_instance_t;
                                    psz_name : PAnsiChar;
                                    psz_output : PAnsiChar;
                                    p_exception : Plibvlc_exception_t);
    procedure libvlc_vlm_set_input(p_instance : Plibvlc_instance_t;
                                   psz_name : PAnsiChar;
                                   psz_input : PAnsiChar;
                                   p_exception : Plibvlc_exception_t);
    procedure libvlc_vlm_add_input(p_instance : Plibvlc_instance_t;
                                   psz_name : PAnsiChar;
                                   pst_input : PAnsiChar;
                                   p_exception : Plibvlc_exception_t);
    procedure libvlc_vlm_set_loop(p_instance : Plibvlc_instance_t;
                                  psz_name : PAnsiChar;
                                  b_loop : Integer;
                                  p_exception : Plibvlc_exception_t);
    procedure libvlc_vlm_set_mux(p_instance : Plibvlc_instance_t;
                                 psz_name : PAnsiChar;
                                 psz_mux : PAnsiChar;
                                 p_exception : Plibvlc_exception_t);
    procedure libvlc_vlm_change_media(p_instance : Plibvlc_instance_t;
                                      psz_name,
                                      psz_input,
                                      psz_output : PAnsiChar;
                                      i_options : Integer;
                                      ppsz_options : Pointer;
                                      b_enabled : Integer;
                                      b_loop : Integer;
                                      p_exception : Plibvlc_exception_t);
    procedure libvlc_vlm_play_media(p_instance : Plibvlc_instance_t;
                                    psz_name : PAnsiChar;
                                    p_exception : Plibvlc_exception_t);
    procedure libvlc_vlm_stop_media(p_instance : Plibvlc_instance_t;
                                    psz_name : PAnsiChar;
                                    p_exception : Plibvlc_exception_t);
    procedure libvlc_vlm_pause_media(p_instance : Plibvlc_instance_t;
                                     psz_name : PAnsiChar;
                                     p_exception : Plibvlc_exception_t);
    procedure libvlc_vlm_seek_media(p_instance : Plibvlc_instance_t;
                                    psz_name : PAnsiChar;
                                    f_percentage : Single;
                                    p_exception : Plibvlc_exception_t);
    function libvlc_vlm_show_media(p_instance : Plibvlc_instance_t;
                                   psz_name : PAnsiChar;
                                   p_exception : Plibvlc_exception_t) : PAnsiChar;
    function libvlc_vlm_get_media_instance_position(p_instance : Plibvlc_instance_t;
                                                    psz_name : PAnsiChar;
                                                    i_instance : Integer;
                                                    p_exception : Plibvlc_exception_t) : Single;
    function libvlc_vlm_get_media_instance_time(p_instance : Plibvlc_instance_t;
                                                psz_name : PAnsiChar;
                                                i_instance : Integer;
                                                p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_vlm_get_media_instance_length(p_instance : Plibvlc_instance_t;
                                                  psz_name : PAnsiChar;
                                                  i_instance : Integer;
                                                  p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_vlm_get_media_instance_rate(p_instance : Plibvlc_instance_t;
                                                psz_name : PAnsiChar;
                                                i_instance : Integer;
                                                p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_vlm_get_media_instance_title(p_instance : Plibvlc_instance_t;
                                                 psz_name : PAnsiChar;
                                                 i_instance : Integer;
                                                 p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_vlm_get_media_instance_chapter(p_instance : Plibvlc_instance_t;
                                                   psz_name : PAnsiChar;
                                                   i_instance : Integer;
                                                   p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_vlm_get_media_instance_seekable(p_instance : Plibvlc_instance_t;
                                                    psz_name : PAnsiChar;
                                                    i_instance : Integer;
                                                    p_exception : Plibvlc_exception_t) : Integer;
  end;

  ELibVLCNotSupported = type Exception;

function LoadLibVLC(ALibraryName : String = 'libvlc.dll') : ILibVLC;

implementation

type


  Tlibvlc_exception_init = procedure(p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_exception_raised = function(p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_exception_raise = procedure(p_exception : Plibvlc_exception_t;
                                      psz_format : PAnsiChar;
                                      var AData); cdecl;
  Tlibvlc_exception_clear = procedure(p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_exception_get_message = function (p_exception : Plibvlc_exception_t) : PAnsiChar; cdecl;

  Tlibvlc_new = function(argc : Integer;
                         argv : PPAnsiChar;
                         p_exception : Plibvlc_exception_t) : Plibvlc_instance_t; cdecl;
  Tlibvlc_release = procedure(p_instance : Plibvlc_instance_t); cdecl;
  Tlibvlc_retain = procedure(p_instance : Plibvlc_instance_t); cdecl;
  Tlibvlc_add_intf = procedure(p_instalce : Plibvlc_instance_t;
                               name : PAnsiChar;
                               p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_wait = procedure(p_instance : Plibvlc_instance_t); cdecl;
  Tlibvlc_get_version = function() : PAnsiChar; cdecl;
  Tlibvlc_get_compiler = function() : PAnsiChar; cdecl;
  Tlibvlc_get_changeset = function() : PAnsiChar; cdecl;
  Tlibvlc_free = procedure(AData : Pointer); cdecl;

  Tlibvlc_event_attach = procedure(p_event_manager : Plibvlc_event_manager_t;
                                   event_type : libvlc_event_type_t;
                                   f_callback : libvlc_callback_t;
                                   userdata : Pointer;
                                   p_e : Plibvlc_exception_t); cdecl;
  Tlibvlc_event_detach = procedure(p_event_manager : Plibvlc_event_manager_t;
                                   event_type : libvlc_event_type_t;
                                   f_callback : libvlc_callback_t;
                                   userdata : Pointer;
                                   p_e : Plibvlc_exception_t); cdecl;
  Tlibvlc_event_type_name = function(event_type : libvlc_event_type_t) : PAnsiChar; cdecl;

  Tlibvlc_get_log_verbosity = function(p_instance : Plibvlc_instance_t;
                                       p_e : Plibvlc_exception_t) : Cardinal; cdecl;
  Tlibvlc_set_log_verbosity = procedure(p_instance : Plibvlc_instance_t;
                                        level : Cardinal;
                                        p_e : Plibvlc_exception_t); cdecl;
  Tlibvlc_log_open = function(p_instance : Plibvlc_instance_t;
                              p_exception : Plibvlc_exception_t) : Plibvlc_log_t; cdecl;
  Tlibvlc_log_close = procedure(p_log : Plibvlc_log_t;
                                p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_log_count = function(p_log : Plibvlc_log_t;
                               p_exception : Plibvlc_exception_t) : Cardinal; cdecl;
  Tlibvlc_log_clear = procedure(p_log : Plibvlc_log_t;
                                p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_log_get_iterator = function(p_log : Plibvlc_log_t;
                                      p_exception : Plibvlc_exception_t) : Plibvlc_log_iterator_t; cdecl;
  Tlibvlc_log_iterator_free = procedure(p_iter : Plibvlc_log_iterator_t;
                                        p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_log_iterator_has_next = function(p_iter : Plibvlc_log_iterator_t;
                                           p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_log_iterator_next = function(p_iter : Plibvlc_log_iterator_t;
                                       p_buffer : Plibvlc_log_message_t;
                                       p_exception : Plibvlc_exception_t) : Plibvlc_log_message_t; cdecl;

  Tlibvlc_media_new = function(p_instance : Plibvlc_instance_t;
                               psz_mrl : PAnsiChar;
                               p_exception : Plibvlc_exception_t) : Plibvlc_media_t; cdecl;
  Tlibvlc_media_new_as_node = function(p_instance : Plibvlc_instance_t;
                                       psz_name : PAnsiChar;
                                       p_exception : Plibvlc_exception_t) : Plibvlc_media_t; cdecl;
  Tlibvlc_media_add_option = procedure(p_media : Plibvlc_media_t;
                                       ppsz_options : PAnsiChar;
                                       p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_add_option_untrusted = procedure(p_media : Plibvlc_media_t;
                                                 ppsz_options : PAnsiChar;
                                                 p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_retain = procedure(p_media : Plibvlc_media_t); cdecl;
  Tlibvlc_media_release = procedure(p_media : Plibvlc_media_t); cdecl;
  Tlibvlc_media_get_mrl = function(p_media : Plibvlc_media_t;
                                   p_exception : Plibvlc_exception_t) : PAnsiChar; cdecl;
  Tlibvlc_media_duplicate = function(p_media : Plibvlc_media_t) : Plibvlc_media_t; cdecl;
  Tlibvlc_media_get_meta = function(p_media : Plibvlc_media_t;
                                    e_meta : libvlc_meta_t;
                                    p_exception : Plibvlc_exception_t) : PAnsiChar; cdecl;
  Tlibvlc_media_get_state = function(p_media : Plibvlc_media_t;
                                     p_exception : Plibvlc_exception_t) : libvlc_state_t; cdecl;
  Tlibvlc_media_subitems = function(p_media : Plibvlc_media_t;
                                    p_exception : Plibvlc_exception_t) : Plibvlc_media_list_t; cdecl;
  Tlibvlc_media_event_manager = function(p_media : Plibvlc_media_t;
                                         p_exception : Plibvlc_exception_t) : Plibvlc_event_manager_t; cdecl;
  Tlibvlc_media_get_duration = function(p_media : Plibvlc_media_t;
                                        p_exception : Plibvlc_exception_t) : libvlc_time_t; cdecl;
  Tlibvlc_media_is_preparsed = function(p_media : Plibvlc_media_t;
                                        p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_media_set_user_data = procedure(p_media : Plibvlc_media_t;
                                          p_new_user_data : Pointer;
                                          p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_get_user_data = function(p_media : Plibvlc_media_t;
                                         p_exception : Plibvlc_exception_t) : Pointer; cdecl;

  Tlibvlc_media_list_new = function(p_instance : Plibvlc_instance_t;
                                   p_exception : Plibvlc_exception_t) : Plibvlc_media_list_t; cdecl;
  Tlibvlc_media_list_release = procedure(p_media_list : Plibvlc_media_list_t); cdecl;
  Tlibvlc_media_list_retain = procedure(p_media_list : Plibvlc_media_list_t); cdecl;
  Tlibvlc_media_list_set_media = procedure(p_media_list : Plibvlc_media_list_t;
                                           p_media : Plibvlc_media_t;
                                           p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_list_media = function(p_media_list : Plibvlc_media_list_t;
                                      p_exception : Plibvlc_exception_t) : Plibvlc_media_t; cdecl;
  Tlibvlc_media_list_add_media = procedure(p_media_list : Plibvlc_media_list_t;
                                           p_media : Plibvlc_media_t;
                                           p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_list_insert_media = procedure(p_media_list : Plibvlc_media_list_t;
                                              p_media : Plibvlc_media_t;
                                              index : Integer;
                                              p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_list_remove_index = procedure(p_media_list : Plibvlc_media_list_t;
                                              index : Integer;
                                              p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_list_count = function(p_media_list : Plibvlc_media_list_t;
                                      p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_media_list_item_at_index = function(p_media_list : Plibvlc_media_list_t;
                                              index : Integer;
                                              p_exception : Plibvlc_exception_t) : Plibvlc_media_t; cdecl;
  Tlibvlc_media_list_index_of_item = function(p_media_list : Plibvlc_media_list_t;
                                              p_media : Plibvlc_media_t;
                                              p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_media_list_is_readonly = function(p_media_list : Plibvlc_media_list_t) : Integer; cdecl;
  Tlibvlc_media_list_lock = procedure(p_media_list : Plibvlc_media_list_t); cdecl;
  Tlibvlc_media_list_unlock = procedure(p_media_list : Plibvlc_media_list_t); cdecl;
  Tlibvlc_media_list_flat_view = function(p_media_list : Plibvlc_media_list_t;
                                          p_exception : Plibvlc_exception_t) : Plibvlc_media_list_view_t; cdecl;
  Tlibvlc_media_list_hierarchical_view = function(p_media_list : Plibvlc_media_list_t;
                                                  p_exception : Plibvlc_exception_t) : Plibvlc_media_list_view_t; cdecl;
  Tlibvlc_media_list_hierarchical_node_view = function(p_media_list : Plibvlc_media_list_t;
                                                       p_exception : Plibvlc_exception_t) : Plibvlc_media_list_view_t; cdecl;
  Tlibvlc_media_list_event_manager = function(p_media_list : Plibvlc_media_list_t;
                                              p_exception : Plibvlc_exception_t) : Plibvlc_event_manager_t; cdecl;

  Tlibvlc_media_list_view_retain = procedure(p_media_list_view : Plibvlc_media_list_view_t); cdecl;
  Tlibvlc_media_list_view_release = procedure(p_media_list_view : Plibvlc_media_list_view_t); cdecl;
  Tlibvlc_media_list_view_event_manager = function(p_media_list_view : Plibvlc_media_list_view_t) : Plibvlc_event_manager_t; cdecl;
  Tlibvlc_media_list_view_count = function(p_media_list_view : Plibvlc_media_list_view_t;
                                           p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_media_list_view_item_at_index = function(p_media_list_view : Plibvlc_media_list_view_t;
                                                   i_index : Integer;
                                                   p_exception : Plibvlc_exception_t) : Plibvlc_media_t; cdecl;
  Tlibvlc_media_list_view_children_at_index = function(p_media_list_view : Plibvlc_media_list_view_t;
                                                      index : Integer;
                                                      p_exception : Plibvlc_exception_t) : Plibvlc_media_list_view_t; cdecl;
  Tlibvlc_media_list_view_children_for_item = function(p_media_list_view : Plibvlc_media_list_view_t;
                                                       p_media : Plibvlc_media_t;
                                                       p_exception : Plibvlc_exception_t) : Plibvlc_media_list_view_t; cdecl;
  Tlibvlc_media_list_view_index_of_item = function(p_media_list_view : Plibvlc_media_list_view_t;
                                                   p_media : Plibvlc_media_t;
                                                   p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_media_list_view_insert_at_index = procedure(p_media_list_view : Plibvlc_media_list_view_t;
                                                      p_media : Plibvlc_media_t;
                                                      index : Integer;
                                                      p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_list_view_remove_at_index = procedure(p_media_list_view : Plibvlc_media_list_view_t;
                                                      index : Integer;
                                                      p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_list_view_add_item = procedure(p_media_list_view : Plibvlc_media_list_view_t;
                                               p_media : Plibvlc_media_t;
                                               p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_list_view_parent_media_list = function(p_media_list_view : Plibvlc_media_list_view_t;
                                                       p_exception : Plibvlc_exception_t) : Plibvlc_media_list_t; cdecl;

  Tlibvlc_media_library_new = function(p_instance : Plibvlc_instance_t;
                                       p_exception : Plibvlc_exception_t) : Plibvlc_media_library_t; cdecl;
  Tlibvlc_media_library_release = procedure(p_mlib : Plibvlc_media_library_t); cdecl;
  Tlibvlc_media_library_retain = procedure(p_mlib : Plibvlc_media_library_t); cdecl;
  Tlibvlc_media_library_load = procedure(p_mlib : Plibvlc_media_library_t;
                                         p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_library_save = procedure(p_mlib : Plibvlc_media_library_t;
                                         p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_library_media_list = function(p_mlib : Plibvlc_media_library_t;
                                              p_exception : Plibvlc_exception_t) : Plibvlc_media_list_t; cdecl;

  Tlibvlc_media_player_new = function(p_instance : Plibvlc_instance_t;
                                      p_exception : Plibvlc_exception_t) : Plibvlc_media_player_t; cdecl;
  Tlibvlc_media_player_new_from_media = function(p_media : Plibvlc_media_t;
                                                 p_exception : Plibvlc_exception_t) : Plibvlc_media_player_t; cdecl;
  Tlibvlc_media_player_release = procedure(p_media_player : Plibvlc_media_player_t); cdecl;
  Tlibvlc_media_player_retain = procedure(p_media_player : Plibvlc_media_player_t); cdecl;
  Tlibvlc_media_player_set_media = procedure(p_media_player : Plibvlc_media_player_t;
                                             p_media : Plibvlc_media_t;
                                             p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_player_get_media = function(p_media_player : Plibvlc_media_player_t;
                                            p_exception : Plibvlc_exception_t) : Plibvlc_media_t; cdecl;
  Tlibvlc_media_player_event_manager = function(p_media_player : Plibvlc_media_player_t;
                                                p_exception : Plibvlc_exception_t) : Plibvlc_event_manager_t; cdecl;
  Tlibvlc_media_player_is_playing = function(p_media_player : Plibvlc_media_player_t;
                                             p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_media_player_play = procedure(p_media_player : Plibvlc_media_player_t;
                                        p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_player_pause = procedure(p_media_player : Plibvlc_media_player_t;
                                         p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_player_stop = procedure(p_media_player : Plibvlc_media_player_t;
                                        p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_player_set_nsobject = procedure(p_media_player : Plibvlc_media_player_t;
                                                drawable : Pointer;
                                                p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_player_get_nsobject = function(p_media_player : Plibvlc_media_player_t) : Pointer; cdecl;
  Tlibvlc_media_player_set_agl = procedure(p_media_player : Plibvlc_media_player_t;
                                           drawable : Cardinal;
                                           p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_player_get_agl = function(p_media_player : Plibvlc_media_player_t) : Cardinal; cdecl;
  Tlibvlc_media_player_set_xwindow = procedure(p_media_player : Plibvlc_media_player_t;
                                               drawable : Cardinal;
                                               p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_player_get_xwindow = function(p_media_player : Plibvlc_media_player_t) : Cardinal; cdecl;
  Tlibvlc_media_player_set_hwnd = procedure(p_media_player : Plibvlc_media_player_t;
                                            drawable : Pointer;
                                            p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_player_get_hwnd = function(p_media_player : Plibvlc_media_player_t) : Pointer; cdecl;
  Tlibvlc_media_player_get_length = function(p_media_player : Plibvlc_media_player_t;
                                             p_exception : Plibvlc_exception_t) : libvlc_time_t; cdecl;
  Tlibvlc_media_player_get_time = function(p_media_player : Plibvlc_media_player_t;
                                           p_exception : Plibvlc_exception_t) : libvlc_time_t; cdecl;
  Tlibvlc_media_player_set_time = procedure(p_media_player : Plibvlc_media_player_t;
                                            time : libvlc_time_t;
                                            p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_player_get_position = function(p_media_player : Plibvlc_media_player_t;
                                               p_exception : Plibvlc_exception_t) : Single; cdecl;
  Tlibvlc_media_player_set_position = procedure(p_media_player : Plibvlc_media_player_t;
                                                position : Single;
                                                p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_player_set_chapter = procedure(p_media_player : Plibvlc_media_player_t;
                                               chapter : Integer;
                                               p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_player_get_chapter = function(p_media_player : Plibvlc_media_player_t;
                                              p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_media_player_get_chapter_count = function(p_media_player : Plibvlc_media_player_t;
                                                    p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_media_player_will_play = function(p_media_player : Plibvlc_media_player_t;
                                            p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_media_player_get_chapter_count_for_title = function(p_media_player : Plibvlc_media_player_t;
                                                              title : Integer;
                                                              p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_media_player_set_title = procedure(p_media_player : Plibvlc_media_player_t;
                                             title : Integer;
                                             p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_player_get_title = function(p_media_player : Plibvlc_media_player_t;
                                            p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_media_player_get_title_count = function(p_media_player : Plibvlc_media_player_t;
                                                  p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_media_player_previous_chapter = procedure(p_media_player : Plibvlc_media_player_t;
                                                    p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_player_next_chapter = procedure(p_media_player : Plibvlc_media_player_t;
                                                p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_player_get_rate = function(p_media_player : Plibvlc_media_player_t;
                                           p_exception : Plibvlc_exception_t) : Single; cdecl;
  Tlibvlc_media_player_set_rate = procedure(p_media_player : Plibvlc_media_player_t;
                                            rate : Single;
                                            p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_player_get_state = function(p_media_player : Plibvlc_media_player_t;
                                            p_exception : Plibvlc_exception_t) : libvlc_state_t; cdecl;
  Tlibvlc_media_player_get_fps = function(p_media_player : Plibvlc_media_player_t;
                                          p_exception : Plibvlc_exception_t) : Single; cdecl;
  Tlibvlc_media_player_has_vout = function(p_media_player : Plibvlc_media_player_t;
                                           p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_media_player_is_seekable = function(p_media_player : Plibvlc_media_player_t;
                                              p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_media_player_can_pause = function(p_media_player : Plibvlc_media_player_t;
                                            p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_track_description_release = procedure(p_track_description : Plibvlc_track_description_t); cdecl;
  Tlibvlc_toggle_fullscreen = procedure(p_media_player : Plibvlc_media_player_t;
                                        p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_set_fullscreen = procedure(p_media_player : Plibvlc_media_player_t;
                                     enabled : Integer;
                                     p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_get_fullscreen = function(p_media_player : Plibvlc_media_player_t;
                                    p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_video_get_height = function(p_media_player : Plibvlc_media_player_t;
                                      p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_video_get_width = function(p_media_player : Plibvlc_media_player_t;
                                     p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_video_get_scale = function(p_media_player : Plibvlc_media_player_t;
                                     p_exception : Plibvlc_exception_t) : Single; cdecl;
  Tlibvlc_video_set_scale = procedure(p_media_player : Plibvlc_media_player_t;
                                      scale : Single;
                                      p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_video_get_aspect_ratio = function(p_media_player : Plibvlc_media_player_t;
                                            p_exception : Plibvlc_exception_t) : PAnsiChar; cdecl;
  Tlibvlc_video_set_aspect_ratio = procedure(p_media_player : Plibvlc_media_player_t;
                                             psz_aspect : PAnsiChar;
                                             p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_video_get_spu = function(p_media_player : Plibvlc_media_player_t;
                                   p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_video_get_spu_count = function(p_media_player : Plibvlc_media_player_t;
                                         p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_video_get_spu_description = function(p_media_player : Plibvlc_media_player_t;
                                               p_exception : Plibvlc_exception_t) : Plibvlc_track_description_t; cdecl;
  Tlibvlc_video_set_spu = procedure(p_media_player : Plibvlc_media_player_t;
                                    i_spu : Integer;
                                    p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_video_set_subtitle_file = function(p_media_player : Plibvlc_media_player_t;
                                             filename : PAnsiChar;
                                             p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_video_get_title_description = function(p_media_player : Plibvlc_media_player_t;
                                                 p_exception : Plibvlc_exception_t) : Plibvlc_track_description_t; cdecl;
  Tlibvlc_video_get_chapter_description = function(p_media_player : Plibvlc_media_player_t;
                                                   title : Integer;
                                                   p_exception : Plibvlc_exception_t) : Plibvlc_track_description_t; cdecl;
  Tlibvlc_video_get_crop_geometry = function(p_media_player : Plibvlc_media_player_t;
                                             p_exception : Plibvlc_exception_t) : PAnsiChar; cdecl;
  Tlibvlc_video_set_crop_geometry = procedure(p_media_player : Plibvlc_media_player_t;
                                              geometry : PAnsiChar;
                                              p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_toggle_teletext = procedure(p_media_player : Plibvlc_media_player_t;
                                      p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_video_get_teletext = function(p_media_player : Plibvlc_media_player_t;
                                        p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_video_set_teletext = procedure(p_media_player : Plibvlc_media_player_t;
                                         page : Integer;
                                         p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_video_get_track_count = function(p_media_player : Plibvlc_media_player_t;
                                           p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_video_get_track_description = function(p_media_player : Plibvlc_media_player_t;
                                                 p_exception : Plibvlc_exception_t) : Plibvlc_track_description_t; cdecl;
  Tlibvlc_video_get_track = function(p_media_player : Plibvlc_media_player_t;
                                     p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_video_set_track = procedure(p_media_player : Plibvlc_media_player_t;
                                      track : Integer;
                                      p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_video_take_snapshot = procedure(p_media_player : Plibvlc_media_player_t;
                                          filepath : PAnsiChar;
                                          width, height : Integer;
                                          p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_audio_output_list_get = function(p_instance : Plibvlc_instance_t;
                                           p_exception : Plibvlc_exception_t) : Plibvlc_audio_output_t; cdecl;
  Tlibvlc_audio_output_list_release = procedure(audio_output_list : Plibvlc_audio_output_t); cdecl;
  Tlibvlc_audio_output_set = function(p_instance : Plibvlc_instance_t;
                                      psz_audio_output : PAnsiChar) : Integer; cdecl;
  Tlibvlc_audio_output_device_count = function(p_instance : Plibvlc_instance_t;
                                               psz_audio_output : PAnsiChar) : Integer; cdecl;
  Tlibvlc_audio_output_device_longname = function(p_instance : Plibvlc_instance_t;
                                                  psz_audio_output : PAnsiChar;
                                                  device : Integer) : PAnsiChar; cdecl;
  Tlibvlc_audio_output_device_id = function(p_instance : Plibvlc_instance_t;
                                            psz_audio_output : PAnsiChar;
                                            device : Integer) : PAnsiChar; cdecl;
  Tlibvlc_audio_output_device_set = procedure(p_instance : Plibvlc_instance_t;
                                              psz_audio_output : PAnsiChar;
                                              device : PAnsiChar); cdecl;
  Tlibvlc_audio_output_get_device_type = function(p_instance : Plibvlc_instance_t;
                                                  p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_audio_output_set_device_type = procedure(p_instance : Plibvlc_instance_t;
                                                   device_type : Integer;
                                                   p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_audio_toggle_mute = procedure(p_instance : Plibvlc_instance_t;
                                        p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_audio_get_mute = function(p_instance : Plibvlc_instance_t;
                                    p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_audio_set_mute = procedure(p_instance : Plibvlc_instance_t;
                                     status : Integer;
                                     p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_audio_get_volume = function(p_instance : Plibvlc_instance_t;
                                      p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_audio_set_volume = procedure(p_instance : Plibvlc_instance_t;
                                       volume : Integer;
                                       p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_audio_get_track_count = function(p_media_player : Plibvlc_media_player_t;
                                           p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_audio_get_track_description = function(p_media_player : Plibvlc_media_player_t;
                                                 p_exception : Plibvlc_exception_t) : Plibvlc_track_description_t; cdecl;
  Tlibvlc_audio_get_track = function(p_media_player : Plibvlc_media_player_t;
                                     p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_audio_set_track = procedure(p_media_player : Plibvlc_media_player_t;
                                      p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_audio_get_channel = function(p_instance : Plibvlc_instance_t;
                                       p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_audio_set_channel = procedure(p_instance : Plibvlc_instance_t;
                                        channel : Integer;
                                        p_exception : Plibvlc_exception_t); cdecl;

  Tlibvlc_media_list_player_new = function(p_instance : Plibvlc_instance_t;
                                           p_exception : Plibvlc_exception_t) : Plibvlc_media_list_player_t; cdecl;
  Tlibvlc_media_list_player_release = procedure(p_mlp : Plibvlc_media_list_player_t); cdecl;
  Tlibvlc_media_list_player_set_media_player = procedure(p_mlp : Plibvlc_media_list_player_t;
                                                         p_media_player : Plibvlc_media_player_t;
                                                         p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_list_player_set_media_list = procedure(p_mlp : Plibvlc_media_list_player_t;
                                                       p_media_list : Plibvlc_media_list_t;
                                                       p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_list_player_play = procedure(p_mlp : Plibvlc_media_list_player_t;
                                             p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_list_player_pause = procedure(p_mlp : Plibvlc_media_list_player_t;
                                              p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_list_player_is_playing = function(p_mlp : Plibvlc_media_list_player_t;
                                                  p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_media_list_player_get_state = function(p_mlp : Plibvlc_media_list_player_t;
                                                 p_exception : Plibvlc_exception_t) : libvlc_state_t; cdecl;
  Tlibvlc_media_list_player_play_item_at_index = procedure(p_mlp : Plibvlc_media_list_player_t;
                                                           i_index : Integer;
                                                           p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_list_player_play_item = procedure(p_mlp : Plibvlc_media_list_player_t;
                                                  p_media : Plibvlc_media_t;
                                                  p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_list_player_stop = procedure(p_mlp : Plibvlc_media_list_player_t;
                                             p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_media_list_player_next = procedure(p_mlp : Plibvlc_media_list_player_t;
                                             p_exception : Plibvlc_exception_t); cdecl;

  Tlibvlc_media_discoverer_new_from_name = function(p_instance : Plibvlc_instance_t;
                                                    psz_name : PAnsiChar;
                                                    p_exception : Plibvlc_exception_t) : Plibvlc_media_discoverer_t; cdecl;
  Tlibvlc_media_discoverer_release = procedure(p_mdis : Plibvlc_media_discoverer_t); cdecl;
  Tlibvlc_media_discoverer_localized_name = function(p_mdis : Plibvlc_media_discoverer_t) : PAnsiChar; cdecl;
  Tlibvlc_media_discoverer_media_list = function(p_mdis : Plibvlc_media_discoverer_t) : Plibvlc_media_list_t; cdecl;
  Tlibvlc_media_discoverer_event_manager = function(p_mdis : Plibvlc_media_discoverer_t) : Plibvlc_event_manager_t; cdecl;
  Tlibvlc_media_discoverer_is_running = function(p_mdis : Plibvlc_media_discoverer_t) : Integer; cdecl;

  Tlibvlc_vlm_release = procedure(p_instance : Plibvlc_instance_t;
                                  p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_vlm_add_broadcast = procedure(p_instance : Plibvlc_instance_t;
                                        psz_name,
                                        psz_input,
                                        psz_output : PAnsiChar;
                                        options : Integer;
                                        ppsz_options : Pointer;
                                        b_enabled : Integer;
                                        b_loop : Integer;
                                        p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_vlm_add_vod = procedure(p_instance : Plibvlc_instance_t;
                                  psz_name,
                                  psz_input : PAnsiChar;
                                  i_options : Integer;
                                  ppsz_options : Pointer;
                                  b_enabled : Integer;
                                  psz_mux : PAnsiChar;
                                  p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_vlm_del_media = procedure(p_instance : Plibvlc_instance_t;
                                    psz_name : PAnsiChar;
                                    p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_vlm_set_enabled = procedure(p_instance : Plibvlc_instance_t;
                                      psz_name : PAnsiChar;
                                      b_enabled : Integer;
                                      p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_vlm_set_output = procedure(p_instance : Plibvlc_instance_t;
                                     psz_name : PAnsiChar;
                                     psz_output : PAnsiChar;
                                     p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_vlm_set_input = procedure(p_instance : Plibvlc_instance_t;
                                    psz_name : PAnsiChar;
                                    psz_input : PAnsiChar;
                                    p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_vlm_add_input = procedure(p_instance : Plibvlc_instance_t;
                                    psz_name : PAnsiChar;
                                    pst_input : PAnsiChar;
                                    p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_vlm_set_loop = procedure(p_instance : Plibvlc_instance_t;
                                   psz_name : PAnsiChar;
                                   b_loop : Integer;
                                   p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_vlm_set_mux = procedure(p_instance : Plibvlc_instance_t;
                                  psz_name : PAnsiChar;
                                  psz_mux : PAnsiChar;
                                  p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_vlm_change_media = procedure(p_instance : Plibvlc_instance_t;
                                       psz_name,
                                       psz_input,
                                       psz_output : PAnsiChar;
                                       i_options : Integer;
                                       ppsz_options : Pointer;
                                       b_enabled : Integer;
                                       b_loop : Integer;
                                       p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_vlm_play_media = procedure(p_instance : Plibvlc_instance_t;
                                     psz_name : PAnsiChar;
                                     p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_vlm_stop_media = procedure(p_instance : Plibvlc_instance_t;
                                     psz_name : PAnsiChar;
                                     p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_vlm_pause_media = procedure(p_instance : Plibvlc_instance_t;
                                      psz_name : PAnsiChar;
                                      p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_vlm_seek_media = procedure(p_instance : Plibvlc_instance_t;
                                     psz_name : PAnsiChar;
                                     f_percentage : Single;
                                     p_exception : Plibvlc_exception_t); cdecl;
  Tlibvlc_vlm_show_media = function(p_instance : Plibvlc_instance_t;
                                    psz_name : PAnsiChar;
                                    p_exception : Plibvlc_exception_t) : PAnsiChar; cdecl;
  Tlibvlc_vlm_get_media_instance_position = function(p_instance : Plibvlc_instance_t;
                                                     psz_name : PAnsiChar;
                                                     i_instance : Integer;
                                                     p_exception : Plibvlc_exception_t) : Single; cdecl;
  Tlibvlc_vlm_get_media_instance_time = function(p_instance : Plibvlc_instance_t;
                                                 psz_name : PAnsiChar;
                                                 i_instance : Integer;
                                                 p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_vlm_get_media_instance_length = function(p_instance : Plibvlc_instance_t;
                                                   psz_name : PAnsiChar;
                                                   i_instance : Integer;
                                                   p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_vlm_get_media_instance_rate = function(p_instance : Plibvlc_instance_t;
                                                 psz_name : PAnsiChar;
                                                 i_instance : Integer;
                                                 p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_vlm_get_media_instance_title = function(p_instance : Plibvlc_instance_t;
                                                  psz_name : PAnsiChar;
                                                  i_instance : Integer;
                                                  p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_vlm_get_media_instance_chapter = function(p_instance : Plibvlc_instance_t;
                                                    psz_name : PAnsiChar;
                                                    i_instance : Integer;
                                                    p_exception : Plibvlc_exception_t) : Integer; cdecl;
  Tlibvlc_vlm_get_media_instance_seekable = function(p_instance : Plibvlc_instance_t;
                                                     psz_name : PAnsiChar;
                                                     i_instance : Integer;
                                                     p_exception : Plibvlc_exception_t) : Integer; cdecl;

  TLibVLC = class(TInterfacedObject, ILibVLC)
  private
    FLibrary : Cardinal;
    FVersion : Cardinal;

    Flibvlc_exception_init : Tlibvlc_exception_init;
    Flibvlc_exception_raised : Tlibvlc_exception_raised;
    Flibvlc_exception_raise : Tlibvlc_exception_raise;
    Flibvlc_exception_clear : Tlibvlc_exception_clear;
    Flibvlc_exception_get_message : Tlibvlc_exception_get_message;

    Flibvlc_new : Tlibvlc_new;
    Flibvlc_release : Tlibvlc_release;
    Flibvlc_retain : Tlibvlc_retain;
    Flibvlc_add_intf : Tlibvlc_add_intf;
    Flibvlc_wait : Tlibvlc_wait;
    Flibvlc_get_version : Tlibvlc_get_version;
    Flibvlc_get_compiler : Tlibvlc_get_compiler;
    Flibvlc_get_changeset : Tlibvlc_get_changeset;
    Flibvlc_free : Tlibvlc_free;

    Flibvlc_event_attach : Tlibvlc_event_attach;
    Flibvlc_event_detach : Tlibvlc_event_detach;
    Flibvlc_event_type_name : Tlibvlc_event_type_name;

    Flibvlc_get_log_verbosity : Tlibvlc_get_log_verbosity;
    Flibvlc_set_log_verbosity : Tlibvlc_set_log_verbosity;
    Flibvlc_log_open : Tlibvlc_log_open;
    Flibvlc_log_close : Tlibvlc_log_close;
    Flibvlc_log_count : Tlibvlc_log_count;
    Flibvlc_log_clear : Tlibvlc_log_clear;
    Flibvlc_log_get_iterator : Tlibvlc_log_get_iterator;
    Flibvlc_log_iterator_free : Tlibvlc_log_iterator_free;
    Flibvlc_log_iterator_has_next : Tlibvlc_log_iterator_has_next;
    Flibvlc_log_iterator_next : Tlibvlc_log_iterator_next;

    Flibvlc_media_new : Tlibvlc_media_new;
    Flibvlc_media_new_as_node : Tlibvlc_media_new_as_node;
    Flibvlc_media_add_option : Tlibvlc_media_add_option;
    Flibvlc_media_add_option_untrusted : Tlibvlc_media_add_option_untrusted;
    Flibvlc_media_retain : Tlibvlc_media_retain;
    Flibvlc_media_release : Tlibvlc_media_release;
    Flibvlc_media_get_mrl : Tlibvlc_media_get_mrl;
    Flibvlc_media_duplicate : Tlibvlc_media_duplicate;
    Flibvlc_media_get_meta : Tlibvlc_media_get_meta;
    Flibvlc_media_get_state : Tlibvlc_media_get_state;
    Flibvlc_media_subitems : Tlibvlc_media_subitems;
    Flibvlc_media_event_manager : Tlibvlc_media_event_manager;
    Flibvlc_media_get_duration : Tlibvlc_media_get_duration;
    Flibvlc_media_is_preparsed : Tlibvlc_media_is_preparsed;
    Flibvlc_media_set_user_data : Tlibvlc_media_set_user_data;
    Flibvlc_media_get_user_data : Tlibvlc_media_get_user_data;

    Flibvlc_media_list_new : Tlibvlc_media_list_new;
    Flibvlc_media_list_release : Tlibvlc_media_list_release;
    Flibvlc_media_list_retain : Tlibvlc_media_list_retain;
    Flibvlc_media_list_set_media : Tlibvlc_media_list_set_media;
    Flibvlc_media_list_media : Tlibvlc_media_list_media;
    Flibvlc_media_list_add_media : Tlibvlc_media_list_add_media;
    Flibvlc_media_list_insert_media : Tlibvlc_media_list_insert_media;
    Flibvlc_media_list_remove_index : Tlibvlc_media_list_remove_index;
    Flibvlc_media_list_count : Tlibvlc_media_list_count;
    Flibvlc_media_list_item_at_index : Tlibvlc_media_list_item_at_index;
    Flibvlc_media_list_index_of_item : Tlibvlc_media_list_index_of_item;
    Flibvlc_media_list_is_readonly : Tlibvlc_media_list_is_readonly;
    Flibvlc_media_list_lock : Tlibvlc_media_list_lock;
    Flibvlc_media_list_unlock : Tlibvlc_media_list_unlock;
    Flibvlc_media_list_flat_view : Tlibvlc_media_list_flat_view;
    Flibvlc_media_list_hierarchical_view : Tlibvlc_media_list_hierarchical_view;
    Flibvlc_media_list_hierarchical_node_view : Tlibvlc_media_list_hierarchical_node_view;
    Flibvlc_media_list_event_manager : Tlibvlc_media_list_event_manager;

    Flibvlc_media_list_view_retain : Tlibvlc_media_list_view_retain;
    Flibvlc_media_list_view_release : Tlibvlc_media_list_view_release;
    Flibvlc_media_list_view_event_manager : Tlibvlc_media_list_view_event_manager;
    Flibvlc_media_list_view_count : Tlibvlc_media_list_view_count;
    Flibvlc_media_list_view_item_at_index : Tlibvlc_media_list_view_item_at_index;
    Flibvlc_media_list_view_children_at_index : Tlibvlc_media_list_view_children_at_index;
    Flibvlc_media_list_view_children_for_item : Tlibvlc_media_list_view_children_for_item;
    Flibvlc_media_list_view_index_of_item : Tlibvlc_media_list_view_index_of_item;
    Flibvlc_media_list_view_insert_at_index : Tlibvlc_media_list_view_insert_at_index;
    Flibvlc_media_list_view_remove_at_index : Tlibvlc_media_list_view_remove_at_index;
    Flibvlc_media_list_view_add_item : Tlibvlc_media_list_view_add_item;
    Flibvlc_media_list_view_parent_media_list : Tlibvlc_media_list_view_parent_media_list;

    Flibvlc_media_library_new : Tlibvlc_media_library_new;
    Flibvlc_media_library_release : Tlibvlc_media_library_release;
    Flibvlc_media_library_retain : Tlibvlc_media_library_retain;
    Flibvlc_media_library_load : Tlibvlc_media_library_load;
    Flibvlc_media_library_save : Tlibvlc_media_library_save;
    Flibvlc_media_library_media_list : Tlibvlc_media_library_media_list;

    Flibvlc_media_player_new : Tlibvlc_media_player_new;
    Flibvlc_media_player_new_from_media : Tlibvlc_media_player_new_from_media;
    Flibvlc_media_player_release : Tlibvlc_media_player_release;
    Flibvlc_media_player_retain : Tlibvlc_media_player_retain;
    Flibvlc_media_player_set_media : Tlibvlc_media_player_set_media;
    Flibvlc_media_player_get_media : Tlibvlc_media_player_get_media;
    Flibvlc_media_player_event_manager : Tlibvlc_media_player_event_manager;
    Flibvlc_media_player_is_playing : Tlibvlc_media_player_is_playing;
    Flibvlc_media_player_play : Tlibvlc_media_player_play;
    Flibvlc_media_player_pause : Tlibvlc_media_player_pause;
    Flibvlc_media_player_stop : Tlibvlc_media_player_stop;
    Flibvlc_media_player_set_nsobject : Tlibvlc_media_player_set_nsobject;
    Flibvlc_media_player_get_nsobject : Tlibvlc_media_player_get_nsobject;
    Flibvlc_media_player_set_agl : Tlibvlc_media_player_set_agl;
    Flibvlc_media_player_get_agl : Tlibvlc_media_player_get_agl;
    Flibvlc_media_player_set_xwindow : Tlibvlc_media_player_set_xwindow;
    Flibvlc_media_player_get_xwindow : Tlibvlc_media_player_get_xwindow;
    Flibvlc_media_player_set_hwnd : Tlibvlc_media_player_set_hwnd;
    Flibvlc_media_player_get_hwnd : Tlibvlc_media_player_get_hwnd;
    Flibvlc_media_player_get_length : Tlibvlc_media_player_get_length;
    Flibvlc_media_player_get_time : Tlibvlc_media_player_get_time;
    Flibvlc_media_player_set_time : Tlibvlc_media_player_set_time;
    Flibvlc_media_player_get_position : Tlibvlc_media_player_get_position;
    Flibvlc_media_player_set_position : Tlibvlc_media_player_set_position;
    Flibvlc_media_player_set_chapter : Tlibvlc_media_player_set_chapter;
    Flibvlc_media_player_get_chapter : Tlibvlc_media_player_get_chapter;
    Flibvlc_media_player_get_chapter_count : Tlibvlc_media_player_get_chapter_count;
    Flibvlc_media_player_will_play : Tlibvlc_media_player_will_play;
    Flibvlc_media_player_get_chapter_count_for_title : Tlibvlc_media_player_get_chapter_count_for_title;
    Flibvlc_media_player_set_title : Tlibvlc_media_player_set_title;
    Flibvlc_media_player_get_title : Tlibvlc_media_player_get_title;
    Flibvlc_media_player_get_title_count : Tlibvlc_media_player_get_title_count;
    Flibvlc_media_player_previous_chapter : Tlibvlc_media_player_previous_chapter;
    Flibvlc_media_player_next_chapter : Tlibvlc_media_player_next_chapter;
    Flibvlc_media_player_get_rate : Tlibvlc_media_player_get_rate;
    Flibvlc_media_player_set_rate : Tlibvlc_media_player_set_rate;
    Flibvlc_media_player_get_state : Tlibvlc_media_player_get_state;
    Flibvlc_media_player_get_fps : Tlibvlc_media_player_get_fps;
    Flibvlc_media_player_has_vout : Tlibvlc_media_player_has_vout;
    Flibvlc_media_player_is_seekable : Tlibvlc_media_player_is_seekable;
    Flibvlc_media_player_can_pause : Tlibvlc_media_player_can_pause;
    Flibvlc_track_description_release : Tlibvlc_track_description_release;
    Flibvlc_toggle_fullscreen : Tlibvlc_toggle_fullscreen;
    Flibvlc_set_fullscreen : Tlibvlc_set_fullscreen;
    Flibvlc_get_fullscreen : Tlibvlc_get_fullscreen;
    Flibvlc_video_get_height : Tlibvlc_video_get_height;
    Flibvlc_video_get_width : Tlibvlc_video_get_width;
    Flibvlc_video_get_scale : Tlibvlc_video_get_scale;
    Flibvlc_video_set_scale : Tlibvlc_video_set_scale;
    Flibvlc_video_get_aspect_ratio : Tlibvlc_video_get_aspect_ratio;
    Flibvlc_video_set_aspect_ratio : Tlibvlc_video_set_aspect_ratio;
    Flibvlc_video_get_spu : Tlibvlc_video_get_spu;
    Flibvlc_video_get_spu_count : Tlibvlc_video_get_spu_count;
    Flibvlc_video_get_spu_description : Tlibvlc_video_get_spu_description;
    Flibvlc_video_set_spu : Tlibvlc_video_set_spu;
    Flibvlc_video_set_subtitle_file : Tlibvlc_video_set_subtitle_file;
    Flibvlc_video_get_title_description : Tlibvlc_video_get_title_description;
    Flibvlc_video_get_chapter_description : Tlibvlc_video_get_chapter_description;
    Flibvlc_video_get_crop_geometry : Tlibvlc_video_get_crop_geometry;
    Flibvlc_video_set_crop_geometry : Tlibvlc_video_set_crop_geometry;
    Flibvlc_toggle_teletext : Tlibvlc_toggle_teletext;
    Flibvlc_video_get_teletext : Tlibvlc_video_get_teletext;
    Flibvlc_video_set_teletext : Tlibvlc_video_set_teletext;
    Flibvlc_video_get_track_count : Tlibvlc_video_get_track_count;
    Flibvlc_video_get_track_description : Tlibvlc_video_get_track_description;
    Flibvlc_video_get_track : Tlibvlc_video_get_track;
    Flibvlc_video_set_track : Tlibvlc_video_set_track;
    Flibvlc_video_take_snapshot : Tlibvlc_video_take_snapshot;
    Flibvlc_audio_output_list_get : Tlibvlc_audio_output_list_get;
    Flibvlc_audio_output_list_release : Tlibvlc_audio_output_list_release;
    Flibvlc_audio_output_set : Tlibvlc_audio_output_set;
    Flibvlc_audio_output_device_count : Tlibvlc_audio_output_device_count;
    Flibvlc_audio_output_device_longname : Tlibvlc_audio_output_device_longname;
    Flibvlc_audio_output_device_id : Tlibvlc_audio_output_device_id;
    Flibvlc_audio_output_device_set : Tlibvlc_audio_output_device_set;
    Flibvlc_audio_output_get_device_type : Tlibvlc_audio_output_get_device_type;
    Flibvlc_audio_output_set_device_type : Tlibvlc_audio_output_set_device_type;
    Flibvlc_audio_toggle_mute : Tlibvlc_audio_toggle_mute;
    Flibvlc_audio_get_mute : Tlibvlc_audio_get_mute;
    Flibvlc_audio_set_mute : Tlibvlc_audio_set_mute;
    Flibvlc_audio_get_volume : Tlibvlc_audio_get_volume;
    Flibvlc_audio_set_volume : Tlibvlc_audio_set_volume;
    Flibvlc_audio_get_track_count : Tlibvlc_audio_get_track_count;
    Flibvlc_audio_get_track_description : Tlibvlc_audio_get_track_description;
    Flibvlc_audio_get_track : Tlibvlc_audio_get_track;
    Flibvlc_audio_set_track : Tlibvlc_audio_set_track;
    Flibvlc_audio_get_channel : Tlibvlc_audio_get_channel;
    Flibvlc_audio_set_channel : Tlibvlc_audio_set_channel;

    Flibvlc_media_list_player_new : Tlibvlc_media_list_player_new;
    Flibvlc_media_list_player_release : Tlibvlc_media_list_player_release;
    Flibvlc_media_list_player_set_media_player : Tlibvlc_media_list_player_set_media_player;
    Flibvlc_media_list_player_set_media_list : Tlibvlc_media_list_player_set_media_list;
    Flibvlc_media_list_player_play : Tlibvlc_media_list_player_play;
    Flibvlc_media_list_player_pause : Tlibvlc_media_list_player_pause;
    Flibvlc_media_list_player_is_playing : Tlibvlc_media_list_player_is_playing;
    Flibvlc_media_list_player_get_state : Tlibvlc_media_list_player_get_state;
    Flibvlc_media_list_player_play_item_at_index : Tlibvlc_media_list_player_play_item_at_index;
    Flibvlc_media_list_player_play_item : Tlibvlc_media_list_player_play_item;
    Flibvlc_media_list_player_stop : Tlibvlc_media_list_player_stop;
    Flibvlc_media_list_player_next : Tlibvlc_media_list_player_next;

    Flibvlc_media_discoverer_new_from_name : Tlibvlc_media_discoverer_new_from_name;
    Flibvlc_media_discoverer_release : Tlibvlc_media_discoverer_release;
    Flibvlc_media_discoverer_localized_name : Tlibvlc_media_discoverer_localized_name;
    Flibvlc_media_discoverer_media_list : Tlibvlc_media_discoverer_media_list;
    Flibvlc_media_discoverer_event_manager : Tlibvlc_media_discoverer_event_manager;
    Flibvlc_media_discoverer_is_running : Tlibvlc_media_discoverer_is_running;

    Flibvlc_vlm_release : Tlibvlc_vlm_release;
    Flibvlc_vlm_add_broadcast : Tlibvlc_vlm_add_broadcast;
    Flibvlc_vlm_add_vod : Tlibvlc_vlm_add_vod;
    Flibvlc_vlm_del_media : Tlibvlc_vlm_del_media;
    Flibvlc_vlm_set_enabled : Tlibvlc_vlm_set_enabled;
    Flibvlc_vlm_set_output : Tlibvlc_vlm_set_output;
    Flibvlc_vlm_set_input : Tlibvlc_vlm_set_input;
    Flibvlc_vlm_add_input : Tlibvlc_vlm_add_input;
    Flibvlc_vlm_set_loop : Tlibvlc_vlm_set_loop;
    Flibvlc_vlm_set_mux : Tlibvlc_vlm_set_mux;
    Flibvlc_vlm_change_media : Tlibvlc_vlm_change_media;
    Flibvlc_vlm_play_media : Tlibvlc_vlm_play_media;
    Flibvlc_vlm_stop_media : Tlibvlc_vlm_stop_media;
    Flibvlc_vlm_pause_media : Tlibvlc_vlm_pause_media;
    Flibvlc_vlm_seek_media : Tlibvlc_vlm_seek_media;
    Flibvlc_vlm_show_media : Tlibvlc_vlm_show_media;
    Flibvlc_vlm_get_media_instance_position : Tlibvlc_vlm_get_media_instance_position;
    Flibvlc_vlm_get_media_instance_time : Tlibvlc_vlm_get_media_instance_time;
    Flibvlc_vlm_get_media_instance_length : Tlibvlc_vlm_get_media_instance_length;
    Flibvlc_vlm_get_media_instance_rate : Tlibvlc_vlm_get_media_instance_rate;
    Flibvlc_vlm_get_media_instance_title : Tlibvlc_vlm_get_media_instance_title;
    Flibvlc_vlm_get_media_instance_chapter : Tlibvlc_vlm_get_media_instance_chapter;
    Flibvlc_vlm_get_media_instance_seekable : Tlibvlc_vlm_get_media_instance_seekable;


    procedure ReadVersion;
    function EncodeVersion(V1, V2, V3 : Cardinal) : Cardinal;
    procedure LoadFunctions;

    procedure RaiseNotSupported(AFunction : String);
  public
    constructor Create(ALibraryName : String);
    destructor Destroy(); override;

    procedure libvlc_exception_init(p_exception : Plibvlc_exception_t);
    function libvlc_exception_raised(p_exception : Plibvlc_exception_t) : Integer;
    procedure libvlc_exception_raise(p_exception : Plibvlc_exception_t;
                                     psz_format : PAnsiChar;
                                     var AData);
    procedure libvlc_exception_clear(p_exception : Plibvlc_exception_t);
    function libvlc_exception_get_message(p_exception : Plibvlc_exception_t) : PAnsiChar;

    function libvlc_new(argc : Integer;
                        argv : PPAnsiChar;
                        p_exception : Plibvlc_exception_t) : Plibvlc_instance_t;
    procedure libvlc_release(p_instance : Plibvlc_instance_t);
    procedure libvlc_retain(p_instance : Plibvlc_instance_t);
    procedure libvlc_add_intf(p_instance : Plibvlc_instance_t;
                              name : PAnsiChar;
                              p_exception : Plibvlc_exception_t);
    procedure libvlc_wait(p_instance : Plibvlc_instance_t);
    function libvlc_get_version() : PAnsiChar;
    function libvlc_get_compiler() : PAnsiChar;
    function libvlc_get_changeset() : PAnsiChar;
    procedure libvlc_free(AData : Pointer);

    procedure libvlc_event_attach(p_event_manager : Plibvlc_event_manager_t;
                                  event_type : libvlc_event_type_t;
                                  f_callback : libvlc_callback_t;
                                  userdata : Pointer;
                                  p_e : Plibvlc_exception_t);
    procedure libvlc_event_detach(p_event_manager : Plibvlc_event_manager_t;
                                  event_type : libvlc_event_type_t;
                                  f_callback : libvlc_callback_t;
                                  userdata : Pointer;
                                  p_e : Plibvlc_exception_t);
    function libvlc_event_type_name(event_type : libvlc_event_type_t) : PAnsiChar;

    function libvlc_get_log_verbosity(p_instance : Plibvlc_instance_t;
                                      p_e : Plibvlc_exception_t) : Cardinal;
    procedure libvlc_set_log_verbosity(p_instance : Plibvlc_instance_t;
                                       level : Cardinal;
                                       p_e : Plibvlc_exception_t);
    function libvlc_log_open(p_instance : Plibvlc_instance_t;
                             p_exception : Plibvlc_exception_t) : Plibvlc_log_t;
    procedure libvlc_log_close(p_log : Plibvlc_log_t;
                               p_exception : Plibvlc_exception_t);
    function libvlc_log_count(p_log : Plibvlc_log_t;
                              p_exception : Plibvlc_exception_t) : Cardinal;
    procedure libvlc_log_clear(p_log : Plibvlc_log_t;
                               p_exception : Plibvlc_exception_t);
    function libvlc_log_get_iterator(p_log : Plibvlc_log_t;
                                     p_exception : Plibvlc_exception_t) : Plibvlc_log_iterator_t;
    procedure libvlc_log_iterator_free(p_iter : Plibvlc_log_iterator_t;
                                       p_exception : Plibvlc_exception_t);
    function libvlc_log_iterator_has_next(p_iter : Plibvlc_log_iterator_t;
                                          p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_log_iterator_next(p_iter : Plibvlc_log_iterator_t;
                                      p_buffer : Plibvlc_log_message_t;
                                      p_exception : Plibvlc_exception_t) : Plibvlc_log_message_t;

    function libvlc_media_new(p_instance : Plibvlc_instance_t;
                               psz_mrl : PAnsiChar;
                               p_exception : Plibvlc_exception_t) : Plibvlc_media_t;
    function libvlc_media_new_as_node(p_instance : Plibvlc_instance_t;
                                      psz_name : PAnsiChar;
                                      p_exception : Plibvlc_exception_t) : Plibvlc_media_t;
    procedure libvlc_media_add_option(p_media : Plibvlc_media_t;
                                      ppsz_options : PAnsiChar;
                                      p_exception : Plibvlc_exception_t);
    procedure libvlc_media_add_option_untrusted(p_media : Plibvlc_media_t;
                                                ppsz_options : PAnsiChar;
                                                p_exception : Plibvlc_exception_t);
    procedure libvlc_media_retain(p_media : Plibvlc_media_t);
    procedure libvlc_media_release(p_media : Plibvlc_media_t);
    function libvlc_media_get_mrl(p_media : Plibvlc_media_t;
                                  p_exception : Plibvlc_exception_t) : PAnsiChar;
    function libvlc_media_duplicate(p_media : Plibvlc_media_t) : Plibvlc_media_t;
    function libvlc_media_get_meta(p_media : Plibvlc_media_t;
                                   e_meta : libvlc_meta_t;
                                   p_exception : Plibvlc_exception_t) : PAnsiChar;
    function libvlc_media_get_state(p_media : Plibvlc_media_t;
                                    p_exception : Plibvlc_exception_t) : libvlc_state_t;
    function libvlc_media_subitems(p_media : Plibvlc_media_t;
                                   p_exception : Plibvlc_exception_t) : Plibvlc_media_list_t;
    function libvlc_media_event_manager(p_media : Plibvlc_media_t;
                                        p_exception : Plibvlc_exception_t) : Plibvlc_event_manager_t;
    function libvlc_media_get_duration(p_media : Plibvlc_media_t;
                                       p_exception : Plibvlc_exception_t) : libvlc_time_t;
    function libvlc_media_is_preparsed(p_media : Plibvlc_media_t;
                                       p_exception : Plibvlc_exception_t) : Integer;
    procedure libvlc_media_set_user_data(p_media : Plibvlc_media_t;
                                         p_new_user_data : Pointer;
                                         p_exception : Plibvlc_exception_t);
    function libvlc_media_get_user_data(p_media : Plibvlc_media_t;
                                        p_exception : Plibvlc_exception_t) : Pointer;

    function libvlc_media_list_new(p_instance : Plibvlc_instance_t;
                                   p_exception : Plibvlc_exception_t) : Plibvlc_media_list_t;
    procedure libvlc_media_list_release(p_media_list : Plibvlc_media_list_t);
    procedure libvlc_media_list_retain(p_media_list : Plibvlc_media_list_t);
    procedure libvlc_media_list_set_media(p_media_list : Plibvlc_media_list_t;
                                          p_media : Plibvlc_media_t;
                                          p_exception : Plibvlc_exception_t);
    function libvlc_media_list_media(p_media_list : Plibvlc_media_list_t;
                                     p_exception : Plibvlc_exception_t) : Plibvlc_media_t;
    procedure libvlc_media_list_add_media(p_media_list : Plibvlc_media_list_t;
                                          p_media : Plibvlc_media_t;
                                          p_exception : Plibvlc_exception_t);
    procedure libvlc_media_list_insert_media(p_media_list : Plibvlc_media_list_t;
                                             p_media : Plibvlc_media_t;
                                             index : Integer;
                                             p_exception : Plibvlc_exception_t);
    procedure libvlc_media_list_remove_index(p_media_list : Plibvlc_media_list_t;
                                             index : Integer;
                                             p_exception : Plibvlc_exception_t);
    function libvlc_media_list_count(p_media_list : Plibvlc_media_list_t;
                                     p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_media_list_item_at_index(p_media_list : Plibvlc_media_list_t;
                                             index : Integer;
                                             p_exception : Plibvlc_exception_t) : Plibvlc_media_t;
    function libvlc_media_list_index_of_item(p_media_list : Plibvlc_media_list_t;
                                             p_media : Plibvlc_media_t;
                                             p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_media_list_is_readonly(p_media_list : Plibvlc_media_list_t) : Integer;
    procedure libvlc_media_list_lock(p_media_list : Plibvlc_media_list_t);
    procedure libvlc_media_list_unlock(p_media_list : Plibvlc_media_list_t);
    function libvlc_media_list_flat_view(p_media_list : Plibvlc_media_list_t;
                                         p_exception : Plibvlc_exception_t) : Plibvlc_media_list_view_t;
    function libvlc_media_list_hierarchical_view(p_media_list : Plibvlc_media_list_t;
                                                 p_exception : Plibvlc_exception_t) : Plibvlc_media_list_view_t;
    function libvlc_media_list_hierarchical_node_view(p_media_list : Plibvlc_media_list_t;
                                                      p_exception : Plibvlc_exception_t) : Plibvlc_media_list_view_t;
    function libvlc_media_list_event_manager(p_media_list : Plibvlc_media_list_t;
                                             p_exception : Plibvlc_exception_t) : Plibvlc_event_manager_t;

    procedure libvlc_media_list_view_retain(p_media_list_view : Plibvlc_media_list_view_t);
    procedure libvlc_media_list_view_release(p_media_list_view : Plibvlc_media_list_view_t);
    function libvlc_media_list_view_event_manager(p_media_list_view : Plibvlc_media_list_view_t) : Plibvlc_event_manager_t;
    function libvlc_media_list_view_count(p_media_list_view : Plibvlc_media_list_view_t;
                                          p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_media_list_view_item_at_index(p_media_list_view : Plibvlc_media_list_view_t;
                                                  i_index : Integer;
                                                  p_exception : Plibvlc_exception_t) : Plibvlc_media_t;
    function libvlc_media_list_view_children_at_index(p_media_list_view : Plibvlc_media_list_view_t;
                                                      index : Integer;
                                                      p_exception : Plibvlc_exception_t) : Plibvlc_media_list_view_t;
    function libvlc_media_list_view_children_for_item(p_media_list_view : Plibvlc_media_list_view_t;
                                                      p_media : Plibvlc_media_t;
                                                      p_exception : Plibvlc_exception_t) : Plibvlc_media_list_view_t;
    function libvlc_media_list_view_index_of_item(p_media_list_view : Plibvlc_media_list_view_t;
                                                  p_media : Plibvlc_media_t;
                                                  p_exception : Plibvlc_exception_t) : Integer;
    procedure libvlc_media_list_view_insert_at_index(p_media_list_view : Plibvlc_media_list_view_t;
                                                     p_media : Plibvlc_media_t;
                                                     index : Integer;
                                                     p_exception : Plibvlc_exception_t);
    procedure libvlc_media_list_view_remove_at_index(p_media_list_view : Plibvlc_media_list_view_t;
                                                     index : Integer;
                                                     p_exception : Plibvlc_exception_t);
    procedure libvlc_media_list_view_add_item(p_media_list_view : Plibvlc_media_list_view_t;
                                              p_media : Plibvlc_media_t;
                                              p_exception : Plibvlc_exception_t);
    function libvlc_media_list_view_parent_media_list(p_media_list_view : Plibvlc_media_list_view_t;
                                                      p_exception : Plibvlc_exception_t) : Plibvlc_media_list_t;

    function libvlc_media_library_new(p_instance : Plibvlc_instance_t;
                                      p_exception : Plibvlc_exception_t) : Plibvlc_media_library_t;
    procedure libvlc_media_library_release(p_mlib : Plibvlc_media_library_t);
    procedure libvlc_media_library_retain(p_mlib : Plibvlc_media_library_t);
    procedure libvlc_media_library_load(p_mlib : Plibvlc_media_library_t;
                                        p_exception : Plibvlc_exception_t);
    procedure libvlc_media_library_save(p_mlib : Plibvlc_media_library_t;
                                        p_exception : Plibvlc_exception_t);
    function libvlc_media_library_media_list(p_mlib : Plibvlc_media_library_t;
                                             p_exception : Plibvlc_exception_t) : Plibvlc_media_list_t;

    function libvlc_media_player_new(p_instance : Plibvlc_instance_t;
                                     p_exception : Plibvlc_exception_t) : Plibvlc_media_player_t;
    function libvlc_media_player_new_from_media(p_media : Plibvlc_media_t;
                                                p_exception : Plibvlc_exception_t) : Plibvlc_media_player_t;
    procedure libvlc_media_player_release(p_media_player : Plibvlc_media_player_t);
    procedure libvlc_media_player_retain(p_media_player : Plibvlc_media_player_t);
    procedure libvlc_media_player_set_media(p_media_player : Plibvlc_media_player_t;
                                            p_media : Plibvlc_media_t;
                                            p_exception : Plibvlc_exception_t);
    function libvlc_media_player_get_media(p_media_player : Plibvlc_media_player_t;
                                           p_exception : Plibvlc_exception_t) : Plibvlc_media_t;
    function libvlc_media_player_event_manager(p_media_player : Plibvlc_media_player_t;
                                               p_exception : Plibvlc_exception_t) : Plibvlc_event_manager_t;
    function libvlc_media_player_is_playing(p_media_player : Plibvlc_media_player_t;
                                            p_exception : Plibvlc_exception_t) : Integer;
    procedure libvlc_media_player_play(p_media_player : Plibvlc_media_player_t;
                                       p_exception : Plibvlc_exception_t);
    procedure libvlc_media_player_pause(p_media_player : Plibvlc_media_player_t;
                                        p_exception : Plibvlc_exception_t);
    procedure libvlc_media_player_stop(p_media_player : Plibvlc_media_player_t;
                                       p_exception : Plibvlc_exception_t);
    procedure libvlc_media_player_set_nsobject(p_media_player : Plibvlc_media_player_t;
                                               drawable : Pointer;
                                               p_exception : Plibvlc_exception_t);
    function libvlc_media_player_get_nsobject(p_media_player : Plibvlc_media_player_t) : Pointer;
    procedure libvlc_media_player_set_agl(p_media_player : Plibvlc_media_player_t;
                                          drawable : Cardinal;
                                          p_exception : Plibvlc_exception_t);
    function libvlc_media_player_get_agl(p_media_player : Plibvlc_media_player_t) : Cardinal;
    procedure libvlc_media_player_set_xwindow(p_media_player : Plibvlc_media_player_t;
                                              drawable : Cardinal;
                                              p_exception : Plibvlc_exception_t);
    function libvlc_media_player_get_xwindow(p_media_player : Plibvlc_media_player_t) : Cardinal;
    procedure libvlc_media_player_set_hwnd(p_media_player : Plibvlc_media_player_t;
                                           drawable : Pointer;
                                           p_exception : Plibvlc_exception_t);
    function libvlc_media_player_get_hwnd(p_media_player : Plibvlc_media_player_t) : Pointer;
    function libvlc_media_player_get_length(p_media_player : Plibvlc_media_player_t;
                                            p_exception : Plibvlc_exception_t) : libvlc_time_t;
    function libvlc_media_player_get_time(p_media_player : Plibvlc_media_player_t;
                                          p_exception : Plibvlc_exception_t) : libvlc_time_t;
    procedure libvlc_media_player_set_time(p_media_player : Plibvlc_media_player_t;
                                           time : libvlc_time_t;
                                           p_exception : Plibvlc_exception_t);
    function libvlc_media_player_get_position(p_media_player : Plibvlc_media_player_t;
                                              p_exception : Plibvlc_exception_t) : Single;
    procedure libvlc_media_player_set_position(p_media_player : Plibvlc_media_player_t;
                                               position : Single;
                                               p_exception : Plibvlc_exception_t);
    procedure libvlc_media_player_set_chapter(p_media_player : Plibvlc_media_player_t;
                                              chapter : Integer;
                                              p_exception : Plibvlc_exception_t);
    function libvlc_media_player_get_chapter(p_media_player : Plibvlc_media_player_t;
                                             p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_media_player_get_chapter_count(p_media_player : Plibvlc_media_player_t;
                                                   p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_media_player_will_play(p_media_player : Plibvlc_media_player_t;
                                           p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_media_player_get_chapter_count_for_title(p_media_player : Plibvlc_media_player_t;
                                                             title : Integer;
                                                             p_exception : Plibvlc_exception_t) : Integer;
    procedure libvlc_media_player_set_title(p_media_player : Plibvlc_media_player_t;
                                            title : Integer;
                                            p_exception : Plibvlc_exception_t);
    function libvlc_media_player_get_title(p_media_player : Plibvlc_media_player_t;
                                           p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_media_player_get_title_count(p_media_player : Plibvlc_media_player_t;
                                                 p_exception : Plibvlc_exception_t) : Integer;
    procedure libvlc_media_player_previous_chapter(p_media_player : Plibvlc_media_player_t;
                                                   p_exception : Plibvlc_exception_t);
    procedure libvlc_media_player_next_chapter(p_media_player : Plibvlc_media_player_t;
                                               p_exception : Plibvlc_exception_t);
    function libvlc_media_player_get_rate(p_media_player : Plibvlc_media_player_t;
                                          p_exception : Plibvlc_exception_t) : Single;
    procedure libvlc_media_player_set_rate(p_media_player : Plibvlc_media_player_t;
                                           rate : Single;
                                           p_exception : Plibvlc_exception_t);
    function libvlc_media_player_get_state(p_media_player : Plibvlc_media_player_t;
                                           p_exception : Plibvlc_exception_t) : libvlc_state_t;
    function libvlc_media_player_get_fps(p_media_player : Plibvlc_media_player_t;
                                         p_exception : Plibvlc_exception_t) : Single;
    function libvlc_media_player_has_vout(p_media_player : Plibvlc_media_player_t;
                                           p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_media_player_is_seekable(p_media_player : Plibvlc_media_player_t;
                                             p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_media_player_can_pause(p_media_player : Plibvlc_media_player_t;
                                           p_exception : Plibvlc_exception_t) : Integer;
    procedure libvlc_track_description_release(p_track_description : Plibvlc_track_description_t);
    procedure libvlc_toggle_fullscreen(p_media_player : Plibvlc_media_player_t;
                                       p_exception : Plibvlc_exception_t);
    procedure libvlc_set_fullscreen(p_media_player : Plibvlc_media_player_t;
                                    enabled : Integer;
                                    p_exception : Plibvlc_exception_t);
    function libvlc_get_fullscreen(p_media_player : Plibvlc_media_player_t;
                                   p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_video_get_height(p_media_player : Plibvlc_media_player_t;
                                     p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_video_get_width(p_media_player : Plibvlc_media_player_t;
                                    p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_video_get_scale(p_media_player : Plibvlc_media_player_t;
                                     p_exception : Plibvlc_exception_t) : Single;
    procedure libvlc_video_set_scale(p_media_player : Plibvlc_media_player_t;
                                     scale : Single;
                                     p_exception : Plibvlc_exception_t);
    function libvlc_video_get_aspect_ratio(p_media_player : Plibvlc_media_player_t;
                                           p_exception : Plibvlc_exception_t) : PAnsiChar;
    procedure libvlc_video_set_aspect_ratio(p_media_player : Plibvlc_media_player_t;
                                            psz_aspect : PAnsiChar;
                                            p_exception : Plibvlc_exception_t);
    function libvlc_video_get_spu(p_media_player : Plibvlc_media_player_t;
                                  p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_video_get_spu_count(p_media_player : Plibvlc_media_player_t;
                                        p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_video_get_spu_description(p_media_player : Plibvlc_media_player_t;
                                              p_exception : Plibvlc_exception_t) : Plibvlc_track_description_t;
    procedure libvlc_video_set_spu(p_media_player : Plibvlc_media_player_t;
                                   i_spu : Integer;
                                   p_exception : Plibvlc_exception_t);
    function libvlc_video_set_subtitle_file(p_media_player : Plibvlc_media_player_t;
                                            filename : PAnsiChar;
                                            p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_video_get_title_description(p_media_player : Plibvlc_media_player_t;
                                                p_exception : Plibvlc_exception_t) : Plibvlc_track_description_t;
    function libvlc_video_get_chapter_description(p_media_player : Plibvlc_media_player_t;
                                                  title : Integer;
                                                  p_exception : Plibvlc_exception_t) : Plibvlc_track_description_t;
    function libvlc_video_get_crop_geometry(p_media_player : Plibvlc_media_player_t;
                                            p_exception : Plibvlc_exception_t) : PAnsiChar;
    procedure libvlc_video_set_crop_geometry(p_media_player : Plibvlc_media_player_t;
                                             geometry : PAnsiChar;
                                             p_exception : Plibvlc_exception_t);
    procedure libvlc_toggle_teletext(p_media_player : Plibvlc_media_player_t;
                                     p_exception : Plibvlc_exception_t);
    function libvlc_video_get_teletext(p_media_player : Plibvlc_media_player_t;
                                       p_exception : Plibvlc_exception_t) : Integer;
    procedure libvlc_video_set_teletext(p_media_player : Plibvlc_media_player_t;
                                        page : Integer;
                                        p_exception : Plibvlc_exception_t);
    function libvlc_video_get_track_count(p_media_player : Plibvlc_media_player_t;
                                          p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_video_get_track_description(p_media_player : Plibvlc_media_player_t;
                                                p_exception : Plibvlc_exception_t) : Plibvlc_track_description_t;
    function libvlc_video_get_track(p_media_player : Plibvlc_media_player_t;
                                    p_exception : Plibvlc_exception_t) : Integer;
    procedure libvlc_video_set_track(p_media_player : Plibvlc_media_player_t;
                                     track : Integer;
                                     p_exception : Plibvlc_exception_t);
    procedure libvlc_video_take_snapshot(p_media_player : Plibvlc_media_player_t;
                                         filepath : PAnsiChar;
                                         width, height : Integer;
                                         p_exception : Plibvlc_exception_t);
    function libvlc_audio_output_list_get(p_instance : Plibvlc_instance_t;
                                          p_exception : Plibvlc_exception_t) : Plibvlc_audio_output_t;
    procedure libvlc_audio_output_list_release(audio_output_list : Plibvlc_audio_output_t);
    function libvlc_audio_output_set(p_instance : Plibvlc_instance_t;
                                     psz_audio_output : PAnsiChar) : Integer;
    function libvlc_audio_output_device_count(p_instance : Plibvlc_instance_t;
                                              psz_audio_output : PAnsiChar) : Integer;
    function libvlc_audio_output_device_longname(p_instance : Plibvlc_instance_t;
                                                 psz_audio_output : PAnsiChar;
                                                 device : Integer) : PAnsiChar;
    function libvlc_audio_output_device_id(p_instance : Plibvlc_instance_t;
                                           psz_audio_output : PAnsiChar;
                                           device : Integer) : PAnsiChar;
    procedure libvlc_audio_output_device_set(p_instance : Plibvlc_instance_t;
                                              psz_audio_output : PAnsiChar;
                                              device : PAnsiChar);
    function libvlc_audio_output_get_device_type(p_instance : Plibvlc_instance_t;
                                                 p_exception : Plibvlc_exception_t) : Integer;
    procedure libvlc_audio_output_set_device_type(p_instance : Plibvlc_instance_t;
                                                  device_type : Integer;
                                                  p_exception : Plibvlc_exception_t);
    procedure libvlc_audio_toggle_mute(p_instance : Plibvlc_instance_t;
                                       p_exception : Plibvlc_exception_t);
    function libvlc_audio_get_mute(p_instance : Plibvlc_instance_t;
                                   p_exception : Plibvlc_exception_t) : Integer;
    procedure libvlc_audio_set_mute(p_instance : Plibvlc_instance_t;
                                    status : Integer;
                                    p_exception : Plibvlc_exception_t);
    function libvlc_audio_get_volume(p_instance : Plibvlc_instance_t;
                                     p_exception : Plibvlc_exception_t) : Integer;
    procedure libvlc_audio_set_volume(p_instance : Plibvlc_instance_t;
                                      volume : Integer;
                                      p_exception : Plibvlc_exception_t);
    function libvlc_audio_get_track_count(p_media_player : Plibvlc_media_player_t;
                                          p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_audio_get_track_description(p_media_player : Plibvlc_media_player_t;
                                                p_exception : Plibvlc_exception_t) : Plibvlc_track_description_t;
    function libvlc_audio_get_track(p_media_player : Plibvlc_media_player_t;
                                    p_exception : Plibvlc_exception_t) : Integer;
    procedure libvlc_audio_set_track(p_media_player : Plibvlc_media_player_t;
                                     p_exception : Plibvlc_exception_t);
    function libvlc_audio_get_channel(p_instance : Plibvlc_instance_t;
                                      p_exception : Plibvlc_exception_t) : Integer;
    procedure libvlc_audio_set_channel(p_instance : Plibvlc_instance_t;
                                       channel : Integer;
                                       p_exception : Plibvlc_exception_t);

    function libvlc_media_list_player_new(p_instance : Plibvlc_instance_t;
                                          p_exception : Plibvlc_exception_t) : Plibvlc_media_list_player_t;
    procedure libvlc_media_list_player_release(p_mlp : Plibvlc_media_list_player_t);
    procedure libvlc_media_list_player_set_media_player(p_mlp : Plibvlc_media_list_player_t;
                                                        p_media_player : Plibvlc_media_player_t;
                                                        p_exception : Plibvlc_exception_t);
    procedure libvlc_media_list_player_set_media_list(p_mlp : Plibvlc_media_list_player_t;
                                                      p_media_list : Plibvlc_media_list_t;
                                                      p_exception : Plibvlc_exception_t);

    procedure libvlc_media_list_player_play(p_mlp : Plibvlc_media_list_player_t;
                                            p_exception : Plibvlc_exception_t);
    procedure libvlc_media_list_player_pause(p_mlp : Plibvlc_media_list_player_t;
                                             p_exception : Plibvlc_exception_t);
    function libvlc_media_list_player_is_playing(p_mlp : Plibvlc_media_list_player_t;
                                                 p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_media_list_player_get_state(p_mlp : Plibvlc_media_list_player_t;
                                                p_exception : Plibvlc_exception_t) : libvlc_state_t;
    procedure libvlc_media_list_player_play_item_at_index(p_mlp : Plibvlc_media_list_player_t;
                                                          i_index : Integer;
                                                          p_exception : Plibvlc_exception_t);
    procedure libvlc_media_list_player_play_item(p_mlp : Plibvlc_media_list_player_t;
                                                 p_media : Plibvlc_media_t;
                                                 p_exception : Plibvlc_exception_t);
    procedure libvlc_media_list_player_stop(p_mlp : Plibvlc_media_list_player_t;
                                            p_exception : Plibvlc_exception_t);
    procedure libvlc_media_list_player_next(p_mlp : Plibvlc_media_list_player_t;
                                            p_exception : Plibvlc_exception_t);

    function libvlc_media_discoverer_new_from_name(p_instance : Plibvlc_instance_t;
                                                   psz_name : PAnsiChar;
                                                   p_exception : Plibvlc_exception_t) : Plibvlc_media_discoverer_t;
    procedure libvlc_media_discoverer_release(p_mdis : Plibvlc_media_discoverer_t);
    function libvlc_media_discoverer_localized_name(p_mdis : Plibvlc_media_discoverer_t) : PAnsiChar;
    function libvlc_media_discoverer_media_list(p_mdis : Plibvlc_media_discoverer_t) : Plibvlc_media_list_t;
    function libvlc_media_discoverer_event_manager(p_mdis : Plibvlc_media_discoverer_t) : Plibvlc_event_manager_t;
    function libvlc_media_discoverer_is_running(p_mdis : Plibvlc_media_discoverer_t) : Integer;

    procedure libvlc_vlm_release(p_instance : Plibvlc_instance_t;
                                 p_exception : Plibvlc_exception_t);
    procedure libvlc_vlm_add_broadcast(p_instance : Plibvlc_instance_t;
                                       psz_name,
                                       psz_input,
                                       psz_output : PAnsiChar;
                                       options : Integer;
                                       ppsz_options : Pointer;
                                       b_enabled : Integer;
                                       b_loop : Integer;
                                       p_exception : Plibvlc_exception_t);
    procedure libvlc_vlm_add_vod(p_instance : Plibvlc_instance_t;
                                 psz_name,
                                 psz_input : PAnsiChar;
                                 i_options : Integer;
                                 ppsz_options : Pointer;
                                 b_enabled : Integer;
                                 psz_mux : PAnsiChar;
                                 p_exception : Plibvlc_exception_t);

    procedure libvlc_vlm_del_media(p_instance : Plibvlc_instance_t;
                                   psz_name : PAnsiChar;
                                   p_exception : Plibvlc_exception_t);
    procedure libvlc_vlm_set_enabled(p_instance : Plibvlc_instance_t;
                                     psz_name : PAnsiChar;
                                     b_enabled : Integer;
                                     p_exception : Plibvlc_exception_t);
    procedure libvlc_vlm_set_output(p_instance : Plibvlc_instance_t;
                                    psz_name : PAnsiChar;
                                    psz_output : PAnsiChar;
                                    p_exception : Plibvlc_exception_t);
    procedure libvlc_vlm_set_input(p_instance : Plibvlc_instance_t;
                                   psz_name : PAnsiChar;
                                   psz_input : PAnsiChar;
                                   p_exception : Plibvlc_exception_t);
    procedure libvlc_vlm_add_input(p_instance : Plibvlc_instance_t;
                                   psz_name : PAnsiChar;
                                   pst_input : PAnsiChar;
                                   p_exception : Plibvlc_exception_t);
    procedure libvlc_vlm_set_loop(p_instance : Plibvlc_instance_t;
                                  psz_name : PAnsiChar;
                                  b_loop : Integer;
                                  p_exception : Plibvlc_exception_t);
    procedure libvlc_vlm_set_mux(p_instance : Plibvlc_instance_t;
                                 psz_name : PAnsiChar;
                                 psz_mux : PAnsiChar;
                                 p_exception : Plibvlc_exception_t);
    procedure libvlc_vlm_change_media(p_instance : Plibvlc_instance_t;
                                      psz_name,
                                      psz_input,
                                      psz_output : PAnsiChar;
                                      i_options : Integer;
                                      ppsz_options : Pointer;
                                      b_enabled : Integer;
                                      b_loop : Integer;
                                      p_exception : Plibvlc_exception_t);
    procedure libvlc_vlm_play_media(p_instance : Plibvlc_instance_t;
                                    psz_name : PAnsiChar;
                                    p_exception : Plibvlc_exception_t);
    procedure libvlc_vlm_stop_media(p_instance : Plibvlc_instance_t;
                                    psz_name : PAnsiChar;
                                    p_exception : Plibvlc_exception_t);
    procedure libvlc_vlm_pause_media(p_instance : Plibvlc_instance_t;
                                     psz_name : PAnsiChar;
                                     p_exception : Plibvlc_exception_t);
    procedure libvlc_vlm_seek_media(p_instance : Plibvlc_instance_t;
                                    psz_name : PAnsiChar;
                                    f_percentage : Single;
                                    p_exception : Plibvlc_exception_t);
    function libvlc_vlm_show_media(p_instance : Plibvlc_instance_t;
                                   psz_name : PAnsiChar;
                                   p_exception : Plibvlc_exception_t) : PAnsiChar;
    function libvlc_vlm_get_media_instance_position(p_instance : Plibvlc_instance_t;
                                                    psz_name : PAnsiChar;
                                                    i_instance : Integer;
                                                    p_exception : Plibvlc_exception_t) : Single;
    function libvlc_vlm_get_media_instance_time(p_instance : Plibvlc_instance_t;
                                                psz_name : PAnsiChar;
                                                i_instance : Integer;
                                                p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_vlm_get_media_instance_length(p_instance : Plibvlc_instance_t;
                                                  psz_name : PAnsiChar;
                                                  i_instance : Integer;
                                                  p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_vlm_get_media_instance_rate(p_instance : Plibvlc_instance_t;
                                                psz_name : PAnsiChar;
                                                i_instance : Integer;
                                                p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_vlm_get_media_instance_title(p_instance : Plibvlc_instance_t;
                                                 psz_name : PAnsiChar;
                                                 i_instance : Integer;
                                                 p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_vlm_get_media_instance_chapter(p_instance : Plibvlc_instance_t;
                                                   psz_name : PAnsiChar;
                                                   i_instance : Integer;
                                                   p_exception : Plibvlc_exception_t) : Integer;
    function libvlc_vlm_get_media_instance_seekable(p_instance : Plibvlc_instance_t;
                                                    psz_name : PAnsiChar;
                                                    i_instance : Integer;
                                                    p_exception : Plibvlc_exception_t) : Integer;
  end;

function LoadLibVLC(ALibraryName : String = 'libvlc.dll') : ILibVLC;
begin
  Result := TLibVLC.Create(ALibraryName) as ILibVLC;
end;

{ TLibVLC }

constructor TLibVLC.Create(ALibraryName: String);
begin
  inherited Create();

  FLibrary := LoadLibrary(PChar(ALibraryName));

  if FLibrary = 0 then
    RaiseLastOSError;

  ReadVersion;
  LoadFunctions;
end;

destructor TLibVLC.Destroy;
begin
  if FLibrary > 0 then
    FreeLibrary(FLibrary);

  inherited;
end;

function TLibVLC.EncodeVersion(V1, V2, V3: Cardinal): Cardinal;
begin
  Result := (V1 * 1000000) + (V2 * 1000) + V3;
end;

procedure TLibVLC.libvlc_add_intf(p_instance: Plibvlc_instance_t;
  name: PAnsiChar; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_add_intf) then
    Flibvlc_add_intf(p_instance, name, p_exception)
  else
    RaiseNotSupported('libvlc_add_intf');
end;

function TLibVLC.libvlc_audio_get_channel(p_instance: Plibvlc_instance_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_audio_get_channel) then
    Result := Flibvlc_audio_get_channel(p_instance, p_exception)
  else
    RaiseNotSupported('libvlc_audio_get_channel');
end;

function TLibVLC.libvlc_audio_get_mute(p_instance: Plibvlc_instance_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_audio_get_mute) then
    Result := Flibvlc_audio_get_mute(p_instance, p_exception)
  else
    RaiseNotSupported('libvlc_audio_get_mute');
end;

function TLibVLC.libvlc_audio_get_track(p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_audio_get_track) then
    Result := Flibvlc_audio_get_track(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_audio_get_track');
end;

function TLibVLC.libvlc_audio_get_track_count(
  p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_audio_get_track_count) then
    Result := Flibvlc_audio_get_track_count(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_audio_get_track_count');
end;

function TLibVLC.libvlc_audio_get_track_description(
  p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Plibvlc_track_description_t;
begin
  if Assigned(Flibvlc_audio_get_track_description) then
    Result := Flibvlc_audio_get_track_description(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_audio_get_track_description');
end;

function TLibVLC.libvlc_audio_get_volume(p_instance: Plibvlc_instance_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_audio_get_volume) then
    Result := Flibvlc_audio_get_volume(p_instance, p_exception)
  else
    RaiseNotSupported('libvlc_audio_get_volume');
end;

function TLibVLC.libvlc_audio_output_device_count(
  p_instance: Plibvlc_instance_t; psz_audio_output: PAnsiChar): Integer;
begin
  if Assigned(Flibvlc_audio_output_device_count) then
    Result := Flibvlc_audio_output_device_count(p_instance, psz_audio_output)
  else
    RaiseNotSupported('libvlc_audio_output_device_count');
end;

function TLibVLC.libvlc_audio_output_device_id(p_instance: Plibvlc_instance_t;
  psz_audio_output: PAnsiChar; device: Integer): PAnsiChar;
begin
  if Assigned(Flibvlc_audio_output_device_id) then
    Result := Flibvlc_audio_output_device_id(p_instance, psz_audio_output, device)
  else
    RaiseNotSupported('libvlc_audio_output_device_id');
end;

function TLibVLC.libvlc_audio_output_device_longname(
  p_instance: Plibvlc_instance_t; psz_audio_output: PAnsiChar;
  device: Integer): PAnsiChar;
begin
  if Assigned(Flibvlc_audio_output_device_longname) then
    Result := Flibvlc_audio_output_device_longname(p_instance, psz_audio_output, device)
  else
    RaiseNotSupported('libvlc_audio_output_device_longname');
end;

procedure TLibVLC.libvlc_audio_output_device_set(p_instance: Plibvlc_instance_t;
  psz_audio_output, device: PAnsiChar);
begin
  if Assigned(Flibvlc_audio_output_device_set) then
    Flibvlc_audio_output_device_set(p_instance, psz_audio_output, device)
  else
    RaiseNotSupported('libvlc_audio_output_device_set');
end;

function TLibVLC.libvlc_audio_output_get_device_type(
  p_instance: Plibvlc_instance_t; p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_audio_output_get_device_type) then
    Result := Flibvlc_audio_output_get_device_type(p_instance, p_exception)
  else
    RaiseNotSupported('libvlc_audio_output_get_device_type');
end;

function TLibVLC.libvlc_audio_output_list_get(p_instance: Plibvlc_instance_t;
  p_exception: Plibvlc_exception_t): Plibvlc_audio_output_t;
begin
  if Assigned(Flibvlc_audio_output_list_get) then
    Result := Flibvlc_audio_output_list_get(p_instance, p_exception)
  else
    RaiseNotSupported('libvlc_audio_output_list_get');
end;

procedure TLibVLC.libvlc_audio_output_list_release(
  audio_output_list: Plibvlc_audio_output_t);
begin
  if Assigned(Flibvlc_audio_output_list_release) then
    Flibvlc_audio_output_list_release(audio_output_list)
  else
    RaiseNotSupported('libvlc_audio_output_list_release');
end;

function TLibVLC.libvlc_audio_output_set(p_instance: Plibvlc_instance_t;
  psz_audio_output: PAnsiChar): Integer;
begin
  if Assigned(Flibvlc_audio_output_set) then
    Result := Flibvlc_audio_output_set(p_instance, psz_audio_output)
  else
    RaiseNotSupported('libvlc_audio_output_set');
end;

procedure TLibVLC.libvlc_audio_output_set_device_type(
  p_instance: Plibvlc_instance_t; device_type: Integer;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_audio_output_set_device_type) then
    Flibvlc_audio_output_set_device_type(p_instance, device_type, p_exception)
  else
    RaiseNotSupported('libvlc_audio_output_set_device_type');
end;

procedure TLibVLC.libvlc_audio_set_channel(p_instance: Plibvlc_instance_t;
  channel: Integer; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_audio_set_channel) then
    Flibvlc_audio_set_channel(p_instance, channel, p_exception)
  else
    RaiseNotSupported('libvlc_audio_set_channel');
end;

procedure TLibVLC.libvlc_audio_set_mute(p_instance: Plibvlc_instance_t;
  status: Integer; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_audio_set_mute) then
    Flibvlc_audio_set_mute(p_instance, status, p_exception)
  else
    RaiseNotSupported('libvlc_audio_set_mute');
end;

procedure TLibVLC.libvlc_audio_set_track(p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_audio_set_track) then
    Flibvlc_audio_set_track(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_audio_set_track');
end;

procedure TLibVLC.libvlc_audio_set_volume(p_instance: Plibvlc_instance_t;
  volume: Integer; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_audio_set_volume) then
    Flibvlc_audio_set_volume(p_instance, volume, p_exception)
  else
    RaiseNotSupported('libvlc_audio_set_volume');
end;

procedure TLibVLC.libvlc_audio_toggle_mute(p_instance: Plibvlc_instance_t;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_audio_toggle_mute) then
    Flibvlc_audio_toggle_mute(p_instance, p_exception)
  else
    RaiseNotSupported('libvlc_audio_toggle_mute');
end;

procedure TLibVLC.libvlc_event_attach(p_event_manager: Plibvlc_event_manager_t;
  event_type: libvlc_event_type_t; f_callback: libvlc_callback_t;
  userdata: Pointer; p_e: Plibvlc_exception_t);
begin
   if Assigned(Flibvlc_event_attach) then
    Flibvlc_event_attach( p_event_manager,  event_type,  f_callback,  userdata,  p_e)
  else
    RaiseNotSupported('libvlc_event_attach');
end;

procedure TLibVLC.libvlc_event_detach(p_event_manager: Plibvlc_event_manager_t;
  event_type: libvlc_event_type_t; f_callback: libvlc_callback_t;
  userdata: Pointer; p_e: Plibvlc_exception_t);
begin
   if Assigned(Flibvlc_event_detach) then
    Flibvlc_event_detach( p_event_manager,  event_type,  f_callback,  userdata,  p_e)
  else
    RaiseNotSupported('libvlc_event_detach');
end;

function TLibVLC.libvlc_event_type_name(
  event_type: libvlc_event_type_t): PAnsiChar;
begin
  if Assigned(Flibvlc_event_type_name) then
    Result := Flibvlc_event_type_name( event_type)
  else
    RaiseNotSupported('libvlc_event_type_name');
end;

procedure TLibVLC.libvlc_exception_clear(p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_exception_clear) then
    Flibvlc_exception_clear(p_exception)
  else
    RaiseNotSupported('libvlc_exception_clear');
end;

function TLibVLC.libvlc_exception_get_message(
  p_exception: Plibvlc_exception_t): PAnsiChar;
begin
  if Assigned(Flibvlc_exception_get_message) then
    Result := Flibvlc_exception_get_message(p_exception)
  else
    RaiseNotSupported('libvlc_exception_get_message');
end;

procedure TLibVLC.libvlc_exception_init(p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_exception_init) then
    Flibvlc_exception_init(p_exception)
  else
    RaiseNotSupported('libvlc_exception_init');
end;

procedure TLibVLC.libvlc_exception_raise(p_exception: Plibvlc_exception_t;
  psz_format: PAnsiChar; var AData);
begin
  if Assigned(Flibvlc_exception_raise) then
    Flibvlc_exception_raise(p_exception, psz_format, AData)
  else
    RaiseNotSupported('libvlc_exception_raise');
end;

function TLibVLC.libvlc_exception_raised(
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_exception_raised) then
    Result := Flibvlc_exception_raised(p_exception)
  else
    RaiseNotSupported('libvlc_exception_raised');
end;

procedure TLibVLC.libvlc_free(AData: Pointer);
begin
  if Assigned(Flibvlc_free) then
    Flibvlc_free(AData)
  else
    RaiseNotSupported('libvlc_free');
end;

function TLibVLC.libvlc_get_changeset: PAnsiChar;
begin
  if Assigned(Flibvlc_get_changeset) then
    Result := Flibvlc_get_changeset()
  else
    RaiseNotSupported('libvlc_get_changeset');
end;

function TLibVLC.libvlc_get_compiler: PAnsiChar;
begin
  if Assigned(Flibvlc_get_compiler) then
    Result := Flibvlc_get_compiler()
  else
    RaiseNotSupported('libvlc_get_compiler');
end;

function TLibVLC.libvlc_get_fullscreen(p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_get_fullscreen) then
    Result := Flibvlc_get_fullscreen(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_get_fullscreen');
end;

function TLibVLC.libvlc_get_log_verbosity(p_instance: Plibvlc_instance_t;
  p_e: Plibvlc_exception_t): Cardinal;
begin
  if Assigned(Flibvlc_get_log_verbosity) then
    Result := Flibvlc_get_log_verbosity( p_instance,  p_e)
  else
    RaiseNotSupported('libvlc_get_log_verbosity');
end;

function TLibVLC.libvlc_get_version: PAnsiChar;
begin
  if Assigned(Flibvlc_get_version) then
    Result := Flibvlc_get_version()
  else
    RaiseNotSupported('libvlc_get_version');
end;

procedure TLibVLC.libvlc_log_clear(p_log: Plibvlc_log_t;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_log_clear) then
    Flibvlc_log_clear(p_log, p_exception)
  else
    RaiseNotSupported('libvlc_log_clear');
end;

procedure TLibVLC.libvlc_log_close(p_log: Plibvlc_log_t;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_log_close) then
    Flibvlc_log_close(p_log, p_exception)
  else
    RaiseNotSupported('libvlc_log_close');
end;

function TLibVLC.libvlc_log_count(p_log: Plibvlc_log_t;
  p_exception: Plibvlc_exception_t): Cardinal;
begin
  if Assigned(Flibvlc_log_count) then
    Result := Flibvlc_log_count(p_log, p_exception)
  else
    RaiseNotSupported('libvlc_log_count');
end;

function TLibVLC.libvlc_log_get_iterator(p_log: Plibvlc_log_t;
  p_exception: Plibvlc_exception_t): Plibvlc_log_iterator_t;
begin
  if Assigned(Flibvlc_log_get_iterator) then
    Result := Flibvlc_log_get_iterator(p_log, p_exception)
  else
    RaiseNotSupported('libvlc_log_get_iterator');
end;

procedure TLibVLC.libvlc_log_iterator_free(p_iter: Plibvlc_log_iterator_t;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_log_iterator_free) then
    Flibvlc_log_iterator_free(p_iter, p_exception)
  else
    RaiseNotSupported('libvlc_log_iterator_free');
end;

function TLibVLC.libvlc_log_iterator_has_next(p_iter: Plibvlc_log_iterator_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_log_iterator_has_next) then
    Result := Flibvlc_log_iterator_has_next(p_iter, p_exception)
  else
    RaiseNotSupported('libvlc_log_iterator_has_next');
end;

function TLibVLC.libvlc_log_iterator_next(p_iter: Plibvlc_log_iterator_t;
  p_buffer: Plibvlc_log_message_t;
  p_exception: Plibvlc_exception_t): Plibvlc_log_message_t;
begin
  if Assigned(Flibvlc_log_iterator_next) then
    Result := Flibvlc_log_iterator_next(p_iter, p_buffer, p_exception)
  else
    RaiseNotSupported('libvlc_log_iterator_next');
end;

function TLibVLC.libvlc_log_open(p_instance: Plibvlc_instance_t;
  p_exception: Plibvlc_exception_t): Plibvlc_log_t;
begin
  if Assigned(Flibvlc_log_open) then
    Result := Flibvlc_log_open(p_instance, p_exception)
  else
    RaiseNotSupported('libvlc_log_open');
end;

procedure TLibVLC.libvlc_media_add_option(p_media: Plibvlc_media_t;
  ppsz_options: PAnsiChar; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_add_option) then
    Flibvlc_media_add_option(p_media, ppsz_options, p_exception)
  else
    RaiseNotSupported('libvlc_media_add_option');
end;

procedure TLibVLC.libvlc_media_add_option_untrusted(p_media: Plibvlc_media_t;
  ppsz_options: PAnsiChar; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_add_option_untrusted) then
    Flibvlc_media_add_option_untrusted(p_media, ppsz_options, p_exception)
  else
    RaiseNotSupported('libvlc_media_add_option_untrusted');
end;

function TLibVLC.libvlc_media_discoverer_event_manager(
  p_mdis: Plibvlc_media_discoverer_t): Plibvlc_event_manager_t;
begin
  if Assigned(Flibvlc_media_discoverer_event_manager) then
    Result := Flibvlc_media_discoverer_event_manager(p_mdis)
  else
    RaiseNotSupported('libvlc_media_discoverer_event_manager');
end;

function TLibVLC.libvlc_media_discoverer_is_running(
  p_mdis: Plibvlc_media_discoverer_t): Integer;
begin
  if Assigned(Flibvlc_media_discoverer_is_running) then
    Result := Flibvlc_media_discoverer_is_running(p_mdis)
  else
    RaiseNotSupported('libvlc_media_discoverer_is_running');
end;

function TLibVLC.libvlc_media_discoverer_localized_name(
  p_mdis: Plibvlc_media_discoverer_t): PAnsiChar;
begin
  if Assigned(Flibvlc_media_discoverer_localized_name) then
    Result := Flibvlc_media_discoverer_localized_name(p_mdis)
  else
    RaiseNotSupported('libvlc_media_discoverer_localized_name');
end;

function TLibVLC.libvlc_media_discoverer_media_list(
  p_mdis: Plibvlc_media_discoverer_t): Plibvlc_media_list_t;
begin
  if Assigned(Flibvlc_media_discoverer_media_list) then
    Result := Flibvlc_media_discoverer_media_list(p_mdis)
  else
    RaiseNotSupported('libvlc_media_discoverer_media_list');
end;

function TLibVLC.libvlc_media_discoverer_new_from_name(
  p_instance: Plibvlc_instance_t; psz_name: PAnsiChar;
  p_exception: Plibvlc_exception_t): Plibvlc_media_discoverer_t;
begin
  if Assigned(Flibvlc_media_discoverer_new_from_name) then
    Result := Flibvlc_media_discoverer_new_from_name(p_instance, psz_name, p_exception)
  else
    RaiseNotSupported('libvlc_media_discoverer_new_from_name');
end;

procedure TLibVLC.libvlc_media_discoverer_release(
  p_mdis: Plibvlc_media_discoverer_t);
begin
  if Assigned(Flibvlc_media_discoverer_release) then
    Flibvlc_media_discoverer_release(p_mdis)
  else
    RaiseNotSupported('libvlc_media_discoverer_release');
end;

function TLibVLC.libvlc_media_duplicate(
  p_media: Plibvlc_media_t): Plibvlc_media_t;
begin
  if Assigned(Flibvlc_media_duplicate) then
    Result := Flibvlc_media_duplicate(p_media)
  else
    RaiseNotSupported('libvlc_media_duplicate');
end;

function TLibVLC.libvlc_media_event_manager(p_media: Plibvlc_media_t;
  p_exception: Plibvlc_exception_t): Plibvlc_event_manager_t;
begin
  if Assigned(Flibvlc_media_event_manager) then
    Result := Flibvlc_media_event_manager(p_media, p_exception)
  else
    RaiseNotSupported('libvlc_media_event_manager');
end;

function TLibVLC.libvlc_media_get_duration(p_media: Plibvlc_media_t;
  p_exception: Plibvlc_exception_t): libvlc_time_t;
begin
  if Assigned(Flibvlc_media_get_duration) then
    Result := Flibvlc_media_get_duration(p_media, p_exception)
  else
    RaiseNotSupported('libvlc_media_get_duration');
end;

function TLibVLC.libvlc_media_get_meta(p_media: Plibvlc_media_t;
  e_meta: libvlc_meta_t; p_exception: Plibvlc_exception_t): PAnsiChar;
begin
  if Assigned(Flibvlc_media_get_meta) then
    Result := Flibvlc_media_get_meta(p_media, e_meta, p_exception)
  else
    RaiseNotSupported('libvlc_media_get_meta');
end;

function TLibVLC.libvlc_media_get_mrl(p_media: Plibvlc_media_t;
  p_exception: Plibvlc_exception_t): PAnsiChar;
begin
  if Assigned(Flibvlc_media_get_mrl) then
    Result := Flibvlc_media_get_mrl(p_media, p_exception)
  else
    RaiseNotSupported('libvlc_media_get_mrl');
end;

function TLibVLC.libvlc_media_get_state(p_media: Plibvlc_media_t;
  p_exception: Plibvlc_exception_t): libvlc_state_t;
begin
  if Assigned(Flibvlc_media_get_state) then
    Result := Flibvlc_media_get_state(p_media, p_exception)
  else
    RaiseNotSupported('libvlc_media_get_state');
end;

function TLibVLC.libvlc_media_get_user_data(p_media: Plibvlc_media_t;
  p_exception: Plibvlc_exception_t): Pointer;
begin
  if Assigned(Flibvlc_media_get_user_data) then
    Result := Flibvlc_media_get_user_data(p_media, p_exception)
  else
    RaiseNotSupported('libvlc_media_get_user_data');
end;

function TLibVLC.libvlc_media_is_preparsed(p_media: Plibvlc_media_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_media_is_preparsed) then
    Result := Flibvlc_media_is_preparsed(p_media, p_exception)
  else
    RaiseNotSupported('libvlc_media_is_preparsed');
end;

procedure TLibVLC.libvlc_media_library_load(p_mlib: Plibvlc_media_library_t;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_library_load) then
    Flibvlc_media_library_load(p_mlib, p_exception)
  else
    RaiseNotSupported('libvlc_media_library_load');
end;

function TLibVLC.libvlc_media_library_media_list(
  p_mlib: Plibvlc_media_library_t;
  p_exception: Plibvlc_exception_t): Plibvlc_media_list_t;
begin
  if Assigned(Flibvlc_media_library_media_list) then
    Result := Flibvlc_media_library_media_list(p_mlib, p_exception)
  else
    RaiseNotSupported('libvlc_media_library_media_list');
end;

function TLibVLC.libvlc_media_library_new(p_instance: Plibvlc_instance_t;
  p_exception: Plibvlc_exception_t): Plibvlc_media_library_t;
begin
  if Assigned(Flibvlc_media_library_new) then
    Result := Flibvlc_media_library_new(p_instance, p_exception)
  else
    RaiseNotSupported('libvlc_media_library_new');
end;

procedure TLibVLC.libvlc_media_library_release(p_mlib: Plibvlc_media_library_t);
begin
  if Assigned(Flibvlc_media_library_release) then
    Flibvlc_media_library_release(p_mlib)
  else
    RaiseNotSupported('libvlc_media_library_release');
end;

procedure TLibVLC.libvlc_media_library_retain(p_mlib: Plibvlc_media_library_t);
begin
  if Assigned(Flibvlc_media_library_retain) then
    Flibvlc_media_library_retain(p_mlib)
  else
    RaiseNotSupported('libvlc_media_library_retain');
end;

procedure TLibVLC.libvlc_media_library_save(p_mlib: Plibvlc_media_library_t;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_library_save) then
    Flibvlc_media_library_save(p_mlib, p_exception)
  else
    RaiseNotSupported('libvlc_media_library_save');
end;

procedure TLibVLC.libvlc_media_list_add_media(
  p_media_list: Plibvlc_media_list_t; p_media: Plibvlc_media_t;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_list_add_media) then
    Flibvlc_media_list_add_media(p_media_list, p_media, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_add_media');
end;

function TLibVLC.libvlc_media_list_count(p_media_list: Plibvlc_media_list_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_media_list_count) then
    Result := Flibvlc_media_list_count(p_media_list, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_count');
end;

function TLibVLC.libvlc_media_list_event_manager(
  p_media_list: Plibvlc_media_list_t;
  p_exception: Plibvlc_exception_t): Plibvlc_event_manager_t;
begin
  if Assigned(Flibvlc_media_list_event_manager) then
    Result := Flibvlc_media_list_event_manager(p_media_list, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_event_manager');
end;

function TLibVLC.libvlc_media_list_flat_view(p_media_list: Plibvlc_media_list_t;
  p_exception: Plibvlc_exception_t): Plibvlc_media_list_view_t;
begin
  if Assigned(Flibvlc_media_list_flat_view) then
    Result := Flibvlc_media_list_flat_view(p_media_list, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_flat_view');
end;

function TLibVLC.libvlc_media_list_hierarchical_node_view(
  p_media_list: Plibvlc_media_list_t;
  p_exception: Plibvlc_exception_t): Plibvlc_media_list_view_t;
begin
  if Assigned(Flibvlc_media_list_hierarchical_node_view) then
    Result := Flibvlc_media_list_hierarchical_node_view(p_media_list, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_hierarchical_node_view');
end;

function TLibVLC.libvlc_media_list_hierarchical_view(
  p_media_list: Plibvlc_media_list_t;
  p_exception: Plibvlc_exception_t): Plibvlc_media_list_view_t;
begin
  if Assigned(Flibvlc_media_list_hierarchical_view) then
    Result := Flibvlc_media_list_hierarchical_view(p_media_list, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_hierarchical_view');
end;

function TLibVLC.libvlc_media_list_index_of_item(
  p_media_list: Plibvlc_media_list_t; p_media: Plibvlc_media_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_media_list_index_of_item) then
    Result := Flibvlc_media_list_index_of_item(p_media_list, p_media, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_index_of_item');
end;

procedure TLibVLC.libvlc_media_list_insert_media(
  p_media_list: Plibvlc_media_list_t; p_media: Plibvlc_media_t;
  index: Integer; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_list_insert_media) then
    Flibvlc_media_list_insert_media(p_media_list, p_media, index, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_insert_media');
end;

function TLibVLC.libvlc_media_list_is_readonly(
  p_media_list: Plibvlc_media_list_t): Integer;
begin
  if Assigned(Flibvlc_media_list_is_readonly) then
    Result := Flibvlc_media_list_is_readonly(p_media_list)
  else
    RaiseNotSupported('libvlc_media_list_is_readonly');
end;

function TLibVLC.libvlc_media_list_item_at_index(
  p_media_list: Plibvlc_media_list_t; index: Integer;
  p_exception: Plibvlc_exception_t): Plibvlc_media_t;
begin
  if Assigned(Flibvlc_media_list_item_at_index) then
    Result := Flibvlc_media_list_item_at_index(p_media_list, index, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_item_at_index');
end;

procedure TLibVLC.libvlc_media_list_lock(p_media_list: Plibvlc_media_list_t);
begin
  if Assigned(Flibvlc_media_list_lock) then
    Flibvlc_media_list_lock(p_media_list)
  else
    RaiseNotSupported('libvlc_media_list_lock');
end;

function TLibVLC.libvlc_media_list_media(p_media_list: Plibvlc_media_list_t;
  p_exception: Plibvlc_exception_t): Plibvlc_media_t;
begin
  if Assigned(Flibvlc_media_list_media) then
    Result := Flibvlc_media_list_media(p_media_list, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_media');
end;

function TLibVLC.libvlc_media_list_new(p_instance: Plibvlc_instance_t;
  p_exception: Plibvlc_exception_t): Plibvlc_media_list_t;
begin
  if Assigned(Flibvlc_media_list_new) then
    Result := Flibvlc_media_list_new(p_instance, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_new');
end;

function TLibVLC.libvlc_media_list_player_get_state(
  p_mlp: Plibvlc_media_list_player_t;
  p_exception: Plibvlc_exception_t): libvlc_state_t;
begin
  if Assigned(Flibvlc_media_list_player_get_state) then
    Result := Flibvlc_media_list_player_get_state(p_mlp, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_player_get_state');
end;

function TLibVLC.libvlc_media_list_player_is_playing(
  p_mlp: Plibvlc_media_list_player_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_media_list_player_is_playing) then
    Result := Flibvlc_media_list_player_is_playing(p_mlp, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_player_is_playing');
end;

function TLibVLC.libvlc_media_list_player_new(p_instance: Plibvlc_instance_t;
  p_exception: Plibvlc_exception_t): Plibvlc_media_list_player_t;
begin
  if Assigned(Flibvlc_media_list_player_new) then
    Result := Flibvlc_media_list_player_new(p_instance, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_player_new');
end;

procedure TLibVLC.libvlc_media_list_player_next(
  p_mlp: Plibvlc_media_list_player_t; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_list_player_next) then
    Flibvlc_media_list_player_next(p_mlp, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_player_next');
end;

procedure TLibVLC.libvlc_media_list_player_pause(
  p_mlp: Plibvlc_media_list_player_t; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_list_player_pause) then
    Flibvlc_media_list_player_pause(p_mlp, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_player_pause');
end;

procedure TLibVLC.libvlc_media_list_player_play(
  p_mlp: Plibvlc_media_list_player_t; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_list_player_play) then
    Flibvlc_media_list_player_play(p_mlp, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_player_play');
end;

procedure TLibVLC.libvlc_media_list_player_play_item(
  p_mlp: Plibvlc_media_list_player_t; p_media: Plibvlc_media_t;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_list_player_play_item) then
    Flibvlc_media_list_player_play_item(p_mlp, p_media, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_player_play_item');
end;

procedure TLibVLC.libvlc_media_list_player_play_item_at_index(
  p_mlp: Plibvlc_media_list_player_t; i_index: Integer;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_list_player_play_item_at_index) then
    Flibvlc_media_list_player_play_item_at_index(p_mlp, i_index, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_player_play_item_at_index');
end;

procedure TLibVLC.libvlc_media_list_player_release(
  p_mlp: Plibvlc_media_list_player_t);
begin
  if Assigned(Flibvlc_media_list_player_release) then
    Flibvlc_media_list_player_release(p_mlp)
  else
    RaiseNotSupported('libvlc_media_list_player_release');
end;

procedure TLibVLC.libvlc_media_list_player_set_media_list(
  p_mlp: Plibvlc_media_list_player_t; p_media_list: Plibvlc_media_list_t;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_list_player_set_media_list) then
    Flibvlc_media_list_player_set_media_list(p_mlp, p_media_list, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_player_set_media_list');
end;

procedure TLibVLC.libvlc_media_list_player_set_media_player(
  p_mlp: Plibvlc_media_list_player_t; p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_list_player_set_media_player) then
    Flibvlc_media_list_player_set_media_player(p_mlp, p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_player_set_media_player');
end;

procedure TLibVLC.libvlc_media_list_player_stop(
  p_mlp: Plibvlc_media_list_player_t; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_list_player_stop) then
    Flibvlc_media_list_player_stop(p_mlp, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_player_stop');
end;

procedure TLibVLC.libvlc_media_list_release(p_media_list: Plibvlc_media_list_t);
begin
  if Assigned(Flibvlc_media_list_release) then
    Flibvlc_media_list_release(p_media_list)
  else
    RaiseNotSupported('libvlc_media_list_release');
end;

procedure TLibVLC.libvlc_media_list_remove_index(
  p_media_list: Plibvlc_media_list_t; index : Integer; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_list_remove_index) then
    Flibvlc_media_list_remove_index(p_media_list, index, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_remove_index');
end;

procedure TLibVLC.libvlc_media_list_retain(p_media_list: Plibvlc_media_list_t);
begin
  if Assigned(Flibvlc_media_list_retain) then
    Flibvlc_media_list_retain(p_media_list)
  else
    RaiseNotSupported('libvlc_media_list_retain');
end;

procedure TLibVLC.libvlc_media_list_set_media(
  p_media_list: Plibvlc_media_list_t; p_media: Plibvlc_media_t;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_list_set_media) then
    Flibvlc_media_list_set_media(p_media_list, p_media, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_set_media');
end;

procedure TLibVLC.libvlc_media_list_unlock(p_media_list: Plibvlc_media_list_t);
begin
  if Assigned(Flibvlc_media_list_unlock) then
    Flibvlc_media_list_unlock(p_media_list)
  else
    RaiseNotSupported('libvlc_media_list_unlock');
end;

procedure TLibVLC.libvlc_media_list_view_add_item(
  p_media_list_view: Plibvlc_media_list_view_t; p_media: Plibvlc_media_t;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_list_view_add_item) then
    Flibvlc_media_list_view_add_item(p_media_list_view, p_media, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_view_add_item');
end;

function TLibVLC.libvlc_media_list_view_children_at_index(
  p_media_list_view: Plibvlc_media_list_view_t; index: Integer;
  p_exception: Plibvlc_exception_t): Plibvlc_media_list_view_t;
begin
  if Assigned(Flibvlc_media_list_view_children_at_index) then
    Result := Flibvlc_media_list_view_children_at_index(p_media_list_view, index, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_view_children_at_index');
end;

function TLibVLC.libvlc_media_list_view_children_for_item(
  p_media_list_view: Plibvlc_media_list_view_t; p_media: Plibvlc_media_t;
  p_exception: Plibvlc_exception_t): Plibvlc_media_list_view_t;
begin
  if Assigned(Flibvlc_media_list_view_children_for_item) then
    Result := Flibvlc_media_list_view_children_for_item(p_media_list_view, p_media, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_view_children_for_item');
end;

function TLibVLC.libvlc_media_list_view_count(
  p_media_list_view: Plibvlc_media_list_view_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_media_list_view_count) then
    Result := Flibvlc_media_list_view_count(p_media_list_view, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_view_count');
end;

function TLibVLC.libvlc_media_list_view_event_manager(
  p_media_list_view: Plibvlc_media_list_view_t): Plibvlc_event_manager_t;
begin
  if Assigned(Flibvlc_media_list_view_event_manager) then
    Result := Flibvlc_media_list_view_event_manager(p_media_list_view)
  else
    RaiseNotSupported('libvlc_media_list_view_event_manager');
end;

function TLibVLC.libvlc_media_list_view_index_of_item(
  p_media_list_view: Plibvlc_media_list_view_t; p_media: Plibvlc_media_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_media_list_view_index_of_item) then
    Result := Flibvlc_media_list_view_index_of_item(p_media_list_view, p_media, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_view_index_of_item');
end;

procedure TLibVLC.libvlc_media_list_view_insert_at_index(
  p_media_list_view: Plibvlc_media_list_view_t; p_media: Plibvlc_media_t;
  index: Integer; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_list_view_insert_at_index) then
    Flibvlc_media_list_view_insert_at_index(p_media_list_view, p_media, index, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_view_insert_at_index');
end;

function TLibVLC.libvlc_media_list_view_item_at_index(
  p_media_list_view: Plibvlc_media_list_view_t; i_index: Integer;
  p_exception: Plibvlc_exception_t): Plibvlc_media_t;
begin
  if Assigned(Flibvlc_media_list_view_item_at_index) then
    Result := Flibvlc_media_list_view_item_at_index(p_media_list_view, i_index, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_view_item_at_index');
end;

function TLibVLC.libvlc_media_list_view_parent_media_list(
  p_media_list_view: Plibvlc_media_list_view_t;
  p_exception: Plibvlc_exception_t): Plibvlc_media_list_t;
begin
  if Assigned(Flibvlc_media_list_view_parent_media_list) then
    Result := Flibvlc_media_list_view_parent_media_list(p_media_list_view, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_view_parent_media_list');
end;

procedure TLibVLC.libvlc_media_list_view_release(
  p_media_list_view: Plibvlc_media_list_view_t);
begin
  if Assigned(Flibvlc_media_list_view_release) then
    Flibvlc_media_list_view_release(p_media_list_view)
  else
    RaiseNotSupported('libvlc_media_list_view_release');
end;

procedure TLibVLC.libvlc_media_list_view_remove_at_index(
  p_media_list_view: Plibvlc_media_list_view_t; index: Integer;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_list_view_remove_at_index) then
    Flibvlc_media_list_view_remove_at_index(p_media_list_view, index, p_exception)
  else
    RaiseNotSupported('libvlc_media_list_view_remove_at_index');
end;

procedure TLibVLC.libvlc_media_list_view_retain(
  p_media_list_view: Plibvlc_media_list_view_t);
begin
  if Assigned(Flibvlc_media_list_view_retain) then
    Flibvlc_media_list_view_retain(p_media_list_view)
  else
    RaiseNotSupported('libvlc_media_list_view_retain');
end;

function TLibVLC.libvlc_media_new(p_instance: Plibvlc_instance_t;
  psz_mrl: PAnsiChar; p_exception: Plibvlc_exception_t): Plibvlc_media_t;
begin
  if Assigned(Flibvlc_media_new) then
    Result := Flibvlc_media_new(p_instance, psz_mrl, p_exception)
  else
    RaiseNotSupported('libvlc_media_new');
end;

function TLibVLC.libvlc_media_new_as_node(p_instance: Plibvlc_instance_t;
  psz_name: PAnsiChar; p_exception: Plibvlc_exception_t): Plibvlc_media_t;
begin
  if Assigned(Flibvlc_media_new_as_node) then
    Result := Flibvlc_media_new_as_node(p_instance, psz_name, p_exception)
  else
    RaiseNotSupported('libvlc_media_new_as_node');
end;

function TLibVLC.libvlc_media_player_can_pause(
  p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_media_player_can_pause) then
    Result := Flibvlc_media_player_can_pause(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_can_pause');
end;

function TLibVLC.libvlc_media_player_event_manager(
  p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Plibvlc_event_manager_t;
begin
  if Assigned(Flibvlc_media_player_event_manager) then
    Result := Flibvlc_media_player_event_manager(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_event_manager');
end;

function TLibVLC.libvlc_media_player_get_agl(
  p_media_player: Plibvlc_media_player_t): Cardinal;
begin
  if Assigned(Flibvlc_media_player_get_agl) then
    Result := Flibvlc_media_player_get_agl(p_media_player)
  else
    RaiseNotSupported('libvlc_media_player_get_agl');
end;

function TLibVLC.libvlc_media_player_get_chapter(
  p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_media_player_get_chapter) then
    Result := Flibvlc_media_player_get_chapter(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_get_chapter');
end;

function TLibVLC.libvlc_media_player_get_chapter_count(
  p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_media_player_get_chapter_count) then
    Result := Flibvlc_media_player_get_chapter_count(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_get_chapter_count');
end;

function TLibVLC.libvlc_media_player_get_chapter_count_for_title(
  p_media_player: Plibvlc_media_player_t; title: Integer;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_media_player_get_chapter_count_for_title) then
    Result := Flibvlc_media_player_get_chapter_count_for_title(p_media_player, title, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_get_chapter_count_for_title');
end;

function TLibVLC.libvlc_media_player_get_fps(
  p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Single;
begin
  if Assigned(Flibvlc_media_player_get_fps) then
    Result := Flibvlc_media_player_get_fps(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_get_fps');
end;

function TLibVLC.libvlc_media_player_get_hwnd(
  p_media_player: Plibvlc_media_player_t): Pointer;
begin
  if Assigned(Flibvlc_media_player_get_hwnd) then
    Result := Flibvlc_media_player_get_hwnd(p_media_player)
  else
    RaiseNotSupported('libvlc_media_player_get_hwnd');
end;

function TLibVLC.libvlc_media_player_get_length(
  p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): libvlc_time_t;
begin
  if Assigned(Flibvlc_media_player_get_length) then
    Result := Flibvlc_media_player_get_length(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_get_length');
end;

function TLibVLC.libvlc_media_player_get_media(
  p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Plibvlc_media_t;
begin
  if Assigned(Flibvlc_media_player_get_media) then
    Result := Flibvlc_media_player_get_media(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_get_media');
end;

function TLibVLC.libvlc_media_player_get_nsobject(
  p_media_player: Plibvlc_media_player_t): Pointer;
begin
  if Assigned(Flibvlc_media_player_get_nsobject) then
    Result := Flibvlc_media_player_get_nsobject(p_media_player)
  else
    RaiseNotSupported('libvlc_media_player_get_nsobject');
end;

function TLibVLC.libvlc_media_player_get_position(
  p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Single;
begin
  if Assigned(Flibvlc_media_player_get_position) then
    Result := Flibvlc_media_player_get_position(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_get_position');
end;

function TLibVLC.libvlc_media_player_get_rate(
  p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Single;
begin
  if Assigned(Flibvlc_media_player_get_rate) then
    Result := Flibvlc_media_player_get_rate(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_get_rate');
end;

function TLibVLC.libvlc_media_player_get_state(
  p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): libvlc_state_t;
begin
  if Assigned(Flibvlc_media_player_get_state) then
    Result := Flibvlc_media_player_get_state(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_get_state');
end;

function TLibVLC.libvlc_media_player_get_time(
  p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): libvlc_time_t;
begin
  if Assigned(Flibvlc_media_player_get_time) then
    Result := Flibvlc_media_player_get_time(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_get_time');
end;

function TLibVLC.libvlc_media_player_get_title(
  p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_media_player_get_title) then
    Result := Flibvlc_media_player_get_title(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_get_title');
end;

function TLibVLC.libvlc_media_player_get_title_count(
  p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_media_player_get_title_count) then
    Result := Flibvlc_media_player_get_title_count(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_get_title_count');
end;

function TLibVLC.libvlc_media_player_get_xwindow(
  p_media_player: Plibvlc_media_player_t): Cardinal;
begin
  if Assigned(Flibvlc_media_player_get_xwindow) then
    Result := Flibvlc_media_player_get_xwindow(p_media_player)
  else
    RaiseNotSupported('libvlc_media_player_get_xwindow');
end;

function TLibVLC.libvlc_media_player_has_vout(
  p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_media_player_has_vout) then
    Result := Flibvlc_media_player_has_vout(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_has_vout');
end;

function TLibVLC.libvlc_media_player_is_playing(
  p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_media_player_is_playing) then
    Result := Flibvlc_media_player_is_playing(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_is_playing');
end;

function TLibVLC.libvlc_media_player_is_seekable(
  p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_media_player_is_seekable) then
    Result := Flibvlc_media_player_is_seekable(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_is_seekable');
end;

function TLibVLC.libvlc_media_player_new(p_instance: Plibvlc_instance_t;
  p_exception: Plibvlc_exception_t): Plibvlc_media_player_t;
begin
  if Assigned(Flibvlc_media_player_new) then
    Result := Flibvlc_media_player_new(p_instance, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_new');
end;

function TLibVLC.libvlc_media_player_new_from_media(p_media: Plibvlc_media_t;
  p_exception: Plibvlc_exception_t): Plibvlc_media_player_t;
begin
  if Assigned(Flibvlc_media_player_new_from_media) then
    Result := Flibvlc_media_player_new_from_media(p_media, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_new_from_media');
end;

procedure TLibVLC.libvlc_media_player_next_chapter(
  p_media_player: Plibvlc_media_player_t; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_player_next_chapter) then
    Flibvlc_media_player_next_chapter(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_next_chapter');
end;

procedure TLibVLC.libvlc_media_player_pause(
  p_media_player: Plibvlc_media_player_t; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_player_pause) then
    Flibvlc_media_player_pause(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_pause');
end;

procedure TLibVLC.libvlc_media_player_play(
  p_media_player: Plibvlc_media_player_t; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_player_play) then
    Flibvlc_media_player_play(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_play');
end;

procedure TLibVLC.libvlc_media_player_previous_chapter(
  p_media_player: Plibvlc_media_player_t; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_player_previous_chapter) then
    Flibvlc_media_player_previous_chapter(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_previous_chapter');
end;

procedure TLibVLC.libvlc_media_player_release(
  p_media_player: Plibvlc_media_player_t);
begin
  if Assigned(Flibvlc_media_player_release) then
    Flibvlc_media_player_release(p_media_player)
  else
    RaiseNotSupported('libvlc_media_player_release');
end;

procedure TLibVLC.libvlc_media_player_retain(
  p_media_player: Plibvlc_media_player_t);
begin
  if Assigned(Flibvlc_media_player_retain) then
    Flibvlc_media_player_retain(p_media_player)
  else
    RaiseNotSupported('libvlc_media_player_retain');
end;

procedure TLibVLC.libvlc_media_player_set_agl(
  p_media_player: Plibvlc_media_player_t; drawable: Cardinal;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_player_set_agl) then
    Flibvlc_media_player_set_agl(p_media_player, drawable, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_set_agl');
end;

procedure TLibVLC.libvlc_media_player_set_chapter(
  p_media_player: Plibvlc_media_player_t; chapter: Integer;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_player_set_chapter) then
    Flibvlc_media_player_set_chapter(p_media_player, chapter, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_set_chapter');
end;

procedure TLibVLC.libvlc_media_player_set_hwnd(
  p_media_player: Plibvlc_media_player_t; drawable: Pointer;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_player_set_hwnd) then
    Flibvlc_media_player_set_hwnd(p_media_player, drawable, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_set_hwnd');
end;

procedure TLibVLC.libvlc_media_player_set_media(
  p_media_player: Plibvlc_media_player_t; p_media: Plibvlc_media_t;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_player_set_media) then
    Flibvlc_media_player_set_media(p_media_player, p_media, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_set_media');
end;

procedure TLibVLC.libvlc_media_player_set_nsobject(
  p_media_player: Plibvlc_media_player_t; drawable: Pointer;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_player_set_nsobject) then
    Flibvlc_media_player_set_nsobject(p_media_player, drawable, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_set_nsobject');
end;

procedure TLibVLC.libvlc_media_player_set_position(
  p_media_player: Plibvlc_media_player_t; position: Single;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_player_set_position) then
    Flibvlc_media_player_set_position(p_media_player, position, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_set_position');
end;

procedure TLibVLC.libvlc_media_player_set_rate(
  p_media_player: Plibvlc_media_player_t; rate: Single;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_player_set_rate) then
    Flibvlc_media_player_set_rate(p_media_player, rate, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_set_rate');
end;

procedure TLibVLC.libvlc_media_player_set_time(
  p_media_player: Plibvlc_media_player_t; time: libvlc_time_t;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_player_set_time) then
    Flibvlc_media_player_set_time(p_media_player, time, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_set_time');
end;

procedure TLibVLC.libvlc_media_player_set_title(
  p_media_player: Plibvlc_media_player_t; title: Integer;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_player_set_title) then
    Flibvlc_media_player_set_title(p_media_player, title, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_set_title');
end;

procedure TLibVLC.libvlc_media_player_set_xwindow(
  p_media_player: Plibvlc_media_player_t; drawable: Cardinal;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_player_set_xwindow) then
    Flibvlc_media_player_set_xwindow(p_media_player, drawable, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_set_xwindow');
end;

procedure TLibVLC.libvlc_media_player_stop(
  p_media_player: Plibvlc_media_player_t; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_player_stop) then
    Flibvlc_media_player_stop(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_stop');
end;

function TLibVLC.libvlc_media_player_will_play(
  p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_media_player_will_play) then
    Result := Flibvlc_media_player_will_play(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_media_player_will_play');
end;

procedure TLibVLC.libvlc_media_release(p_media: Plibvlc_media_t);
begin
  if Assigned(Flibvlc_media_release) then
    Flibvlc_media_release(p_media)
  else
    RaiseNotSupported('libvlc_media_release');
end;

procedure TLibVLC.libvlc_media_retain(p_media: Plibvlc_media_t);
begin
  if Assigned(Flibvlc_media_retain) then
    Flibvlc_media_retain(p_media)
  else
    RaiseNotSupported('libvlc_media_retain');
end;

procedure TLibVLC.libvlc_media_set_user_data(p_media: Plibvlc_media_t;
  p_new_user_data: Pointer; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_media_set_user_data) then
    Flibvlc_media_set_user_data(p_media, p_new_user_data, p_exception)
  else
    RaiseNotSupported('libvlc_media_set_user_data');
end;

function TLibVLC.libvlc_media_subitems(p_media: Plibvlc_media_t;
  p_exception: Plibvlc_exception_t): Plibvlc_media_list_t;
begin
  if Assigned(Flibvlc_media_subitems) then
    Result := Flibvlc_media_subitems(p_media, p_exception)
  else
    RaiseNotSupported('libvlc_media_subitems');
end;

function TLibVLC.libvlc_new(argc: Integer; argv: PPAnsiChar;
  p_exception: Plibvlc_exception_t): Plibvlc_instance_t;
begin
  if Assigned(Flibvlc_new) then
  begin
    {$IFDEF AutoFixFloatingPointOverflowError}
    Set8087CW($133f);
    try
    {$ENDIF}
      Result := Flibvlc_new(argc, argv, p_exception)
    {$IFDEF AutoFixFloatingPointOverflowError}
    finally
      Set8087CW(Default8087CW);
    end;
    {$ENDIF}
  end
  else
    RaiseNotSupported('libvlc_new');
end;

procedure TLibVLC.libvlc_release(p_instance: Plibvlc_instance_t);
begin
  if Assigned(Flibvlc_release) then
    Flibvlc_release(p_instance)
  else
    RaiseNotSupported('libvlc_release');
end;

procedure TLibVLC.libvlc_retain(p_instance: Plibvlc_instance_t);
begin
  if Assigned(Flibvlc_retain) then
    Flibvlc_retain(p_instance)
  else
    RaiseNotSupported('libvlc_retain');
end;

procedure TLibVLC.libvlc_set_fullscreen(p_media_player: Plibvlc_media_player_t;
  enabled: Integer; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_set_fullscreen) then
    Flibvlc_set_fullscreen(p_media_player, enabled, p_exception)
  else
    RaiseNotSupported('libvlc_set_fullscreen');
end;

procedure TLibVLC.libvlc_set_log_verbosity(p_instance: Plibvlc_instance_t;
  level: Cardinal; p_e: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_set_log_verbosity) then
    Flibvlc_set_log_verbosity( p_instance,  level,  p_e)
  else
    RaiseNotSupported('libvlc_set_log_verbosity');
end;

procedure TLibVLC.libvlc_toggle_fullscreen(
  p_media_player: Plibvlc_media_player_t; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_toggle_fullscreen) then
    Flibvlc_toggle_fullscreen(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_toggle_fullscreen');
end;

procedure TLibVLC.libvlc_toggle_teletext(p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_toggle_teletext) then
    Flibvlc_toggle_teletext(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_toggle_teletext');
end;

procedure TLibVLC.libvlc_track_description_release(
  p_track_description: Plibvlc_track_description_t);
begin
  if Assigned(Flibvlc_track_description_release) then
    Flibvlc_track_description_release(p_track_description)
  else
    RaiseNotSupported('libvlc_track_description_release');
end;

function TLibVLC.libvlc_video_get_aspect_ratio(
  p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): PAnsiChar;
begin
  if Assigned(Flibvlc_video_get_aspect_ratio) then
    Result := Flibvlc_video_get_aspect_ratio(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_video_get_aspect_ratio');
end;

function TLibVLC.libvlc_video_get_chapter_description(
  p_media_player: Plibvlc_media_player_t; title: Integer;
  p_exception: Plibvlc_exception_t): Plibvlc_track_description_t;
begin
  if Assigned(Flibvlc_video_get_chapter_description) then
    Result := Flibvlc_video_get_chapter_description(p_media_player, title, p_exception)
  else
    RaiseNotSupported('libvlc_video_get_chapter_description');
end;

function TLibVLC.libvlc_video_get_crop_geometry(
  p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): PAnsiChar;
begin
  if Assigned(Flibvlc_video_get_crop_geometry) then
    Result := Flibvlc_video_get_crop_geometry(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_video_get_crop_geometry');
end;

function TLibVLC.libvlc_video_get_height(p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_video_get_height) then
    Result := Flibvlc_video_get_height(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_video_get_height');
end;

function TLibVLC.libvlc_video_get_scale(p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Single;
begin
  if Assigned(Flibvlc_video_get_scale) then
    Result := Flibvlc_video_get_scale(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_video_get_scale');
end;

function TLibVLC.libvlc_video_get_spu(p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_video_get_spu) then
    Result := Flibvlc_video_get_spu(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_video_get_spu');
end;

function TLibVLC.libvlc_video_get_spu_count(
  p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_video_get_spu_count) then
    Result := Flibvlc_video_get_spu_count(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_video_get_spu_count');
end;

function TLibVLC.libvlc_video_get_spu_description(
  p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Plibvlc_track_description_t;
begin
  if Assigned(Flibvlc_video_get_spu_description) then
    Result := Flibvlc_video_get_spu_description(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_video_get_spu_description');
end;

function TLibVLC.libvlc_video_get_teletext(
  p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_video_get_teletext) then
    Result := Flibvlc_video_get_teletext(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_video_get_teletext');
end;

function TLibVLC.libvlc_video_get_title_description(
  p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Plibvlc_track_description_t;
begin
  if Assigned(Flibvlc_video_get_title_description) then
    Result := Flibvlc_video_get_title_description(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_video_get_title_description');
end;

function TLibVLC.libvlc_video_get_track(p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_video_get_track) then
    Result := Flibvlc_video_get_track(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_video_get_track');
end;

function TLibVLC.libvlc_video_get_track_count(
  p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_video_get_track_count) then
    Result := Flibvlc_video_get_track_count(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_video_get_track_count');
end;

function TLibVLC.libvlc_video_get_track_description(
  p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Plibvlc_track_description_t;
begin
  if Assigned(Flibvlc_video_get_track_description) then
    Result := Flibvlc_video_get_track_description(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_video_get_track_description');
end;

function TLibVLC.libvlc_video_get_width(p_media_player: Plibvlc_media_player_t;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_video_get_width) then
    Result := Flibvlc_video_get_width(p_media_player, p_exception)
  else
    RaiseNotSupported('libvlc_video_get_width');
end;

procedure TLibVLC.libvlc_video_set_aspect_ratio(
  p_media_player: Plibvlc_media_player_t; psz_aspect: PAnsiChar;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_video_set_aspect_ratio) then
    Flibvlc_video_set_aspect_ratio(p_media_player, psz_aspect, p_exception)
  else
    RaiseNotSupported('libvlc_video_set_aspect_ratio');
end;

procedure TLibVLC.libvlc_video_set_crop_geometry(
  p_media_player: Plibvlc_media_player_t; geometry: PAnsiChar;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_video_set_crop_geometry) then
    Flibvlc_video_set_crop_geometry(p_media_player, geometry, p_exception)
  else
    RaiseNotSupported('libvlc_video_set_crop_geometry');
end;

procedure TLibVLC.libvlc_video_set_scale(p_media_player: Plibvlc_media_player_t;
  scale: Single; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_video_set_scale) then
    Flibvlc_video_set_scale(p_media_player, scale, p_exception)
  else
    RaiseNotSupported('libvlc_video_set_scale');
end;

procedure TLibVLC.libvlc_video_set_spu(p_media_player: Plibvlc_media_player_t;
  i_spu: Integer; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_video_set_spu) then
    Flibvlc_video_set_spu(p_media_player, i_spu, p_exception)
  else
    RaiseNotSupported('libvlc_video_set_spu');
end;

function TLibVLC.libvlc_video_set_subtitle_file(
  p_media_player: Plibvlc_media_player_t; filename: PAnsiChar;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_video_set_subtitle_file) then
    Result := Flibvlc_video_set_subtitle_file(p_media_player, filename, p_exception)
  else
    RaiseNotSupported('libvlc_video_set_subtitle_file');
end;

procedure TLibVLC.libvlc_video_set_teletext(
  p_media_player: Plibvlc_media_player_t; page: Integer;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_video_set_teletext) then
    Flibvlc_video_set_teletext(p_media_player, page, p_exception)
  else
    RaiseNotSupported('libvlc_video_set_teletext');
end;

procedure TLibVLC.libvlc_video_set_track(p_media_player: Plibvlc_media_player_t;
  track: Integer; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_video_set_track) then
    Flibvlc_video_set_track(p_media_player, track, p_exception)
  else
    RaiseNotSupported('libvlc_video_set_track');
end;

procedure TLibVLC.libvlc_video_take_snapshot(
  p_media_player: Plibvlc_media_player_t; filepath: PAnsiChar; width,
  height: Integer; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_video_take_snapshot) then
    Flibvlc_video_take_snapshot(p_media_player, filepath, width, height, p_exception)
  else
    RaiseNotSupported('libvlc_video_take_snapshot');
end;

procedure TLibVLC.libvlc_vlm_add_broadcast(p_instance: Plibvlc_instance_t;
  psz_name, psz_input, psz_output: PAnsiChar; options: Integer;
  ppsz_options: Pointer; b_enabled, b_loop: Integer;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_vlm_add_broadcast) then
    Flibvlc_vlm_add_broadcast(p_instance, psz_name, psz_input, psz_output, options, ppsz_options, b_enabled, b_loop, p_exception)
  else
    RaiseNotSupported('libvlc_vlm_add_broadcast');
end;

procedure TLibVLC.libvlc_vlm_add_input(p_instance: Plibvlc_instance_t; psz_name,
  pst_input: PAnsiChar; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_vlm_add_input) then
    Flibvlc_vlm_add_input(p_instance, psz_name, pst_input, p_exception)
  else
    RaiseNotSupported('libvlc_vlm_add_input');
end;

procedure TLibVLC.libvlc_vlm_add_vod(p_instance: Plibvlc_instance_t; psz_name,
  psz_input: PAnsiChar; i_options: Integer; ppsz_options: Pointer;
  b_enabled: Integer; psz_mux: PAnsiChar; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_vlm_add_vod) then
    Flibvlc_vlm_add_vod(p_instance, psz_name, psz_input, i_options, ppsz_options, b_enabled, psz_mux, p_exception)
  else
    RaiseNotSupported('libvlc_vlm_add_vod');
end;

procedure TLibVLC.libvlc_vlm_change_media(p_instance: Plibvlc_instance_t;
  psz_name, psz_input, psz_output: PAnsiChar; i_options: Integer;
  ppsz_options: Pointer; b_enabled, b_loop: Integer;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_vlm_change_media) then
    Flibvlc_vlm_change_media(p_instance, psz_name, psz_input, psz_output, i_options, ppsz_options, b_enabled, b_loop, p_exception)
  else
    RaiseNotSupported('libvlc_vlm_change_media');
end;

procedure TLibVLC.libvlc_vlm_del_media(p_instance: Plibvlc_instance_t;
  psz_name: PAnsiChar; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_vlm_del_media) then
    Flibvlc_vlm_del_media(p_instance, psz_name, p_exception)
  else
    RaiseNotSupported('libvlc_vlm_del_media');
end;

function TLibVLC.libvlc_vlm_get_media_instance_chapter(
  p_instance: Plibvlc_instance_t; psz_name: PAnsiChar; i_instance: Integer;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_vlm_get_media_instance_chapter) then
    Result := Flibvlc_vlm_get_media_instance_chapter(p_instance, psz_name, i_instance, p_exception)
  else
    RaiseNotSupported('libvlc_vlm_get_media_instance_chapter');
end;

function TLibVLC.libvlc_vlm_get_media_instance_length(
  p_instance: Plibvlc_instance_t; psz_name: PAnsiChar; i_instance: Integer;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_vlm_get_media_instance_length) then
    Result := Flibvlc_vlm_get_media_instance_length(p_instance, psz_name, i_instance, p_exception)
  else
    RaiseNotSupported('libvlc_vlm_get_media_instance_length');
end;

function TLibVLC.libvlc_vlm_get_media_instance_position(
  p_instance: Plibvlc_instance_t; psz_name: PAnsiChar; i_instance: Integer;
  p_exception: Plibvlc_exception_t): Single;
begin
  if Assigned(Flibvlc_vlm_get_media_instance_position) then
    Result := Flibvlc_vlm_get_media_instance_position(p_instance, psz_name, i_instance, p_exception)
  else
    RaiseNotSupported('libvlc_vlm_get_media_instance_position');
end;

function TLibVLC.libvlc_vlm_get_media_instance_rate(
  p_instance: Plibvlc_instance_t; psz_name: PAnsiChar; i_instance: Integer;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_vlm_get_media_instance_rate) then
    Result := Flibvlc_vlm_get_media_instance_rate(p_instance, psz_name, i_instance, p_exception)
  else
    RaiseNotSupported('libvlc_vlm_get_media_instance_rate');
end;

function TLibVLC.libvlc_vlm_get_media_instance_seekable(
  p_instance: Plibvlc_instance_t; psz_name: PAnsiChar; i_instance: Integer;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_vlm_get_media_instance_seekable) then
    Result := Flibvlc_vlm_get_media_instance_seekable(p_instance, psz_name, i_instance, p_exception)
  else
    RaiseNotSupported('libvlc_vlm_get_media_instance_seekable');
end;

function TLibVLC.libvlc_vlm_get_media_instance_time(
  p_instance: Plibvlc_instance_t; psz_name: PAnsiChar; i_instance: Integer;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_vlm_get_media_instance_time) then
    Result := Flibvlc_vlm_get_media_instance_time(p_instance, psz_name, i_instance, p_exception)
  else
    RaiseNotSupported('libvlc_vlm_get_media_instance_time');
end;

function TLibVLC.libvlc_vlm_get_media_instance_title(
  p_instance: Plibvlc_instance_t; psz_name: PAnsiChar; i_instance: Integer;
  p_exception: Plibvlc_exception_t): Integer;
begin
  if Assigned(Flibvlc_vlm_get_media_instance_title) then
    Result := Flibvlc_vlm_get_media_instance_title(p_instance, psz_name, i_instance, p_exception)
  else
    RaiseNotSupported('libvlc_vlm_get_media_instance_title');
end;

procedure TLibVLC.libvlc_vlm_pause_media(p_instance: Plibvlc_instance_t;
  psz_name: PAnsiChar; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_vlm_pause_media) then
    Flibvlc_vlm_pause_media(p_instance, psz_name, p_exception)
  else
    RaiseNotSupported('libvlc_vlm_pause_media');
end;

procedure TLibVLC.libvlc_vlm_play_media(p_instance: Plibvlc_instance_t;
  psz_name: PAnsiChar; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_vlm_play_media) then
    Flibvlc_vlm_play_media(p_instance, psz_name, p_exception)
  else
    RaiseNotSupported('libvlc_vlm_play_media');
end;

procedure TLibVLC.libvlc_vlm_release(p_instance: Plibvlc_instance_t;
  p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_vlm_release) then
    Flibvlc_vlm_release(p_instance, p_exception)
  else
    RaiseNotSupported('libvlc_vlm_release');
end;

procedure TLibVLC.libvlc_vlm_seek_media(p_instance: Plibvlc_instance_t;
  psz_name: PAnsiChar; f_percentage: Single; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_vlm_seek_media) then
    Flibvlc_vlm_seek_media(p_instance, psz_name, f_percentage, p_exception)
  else
    RaiseNotSupported('libvlc_vlm_seek_media');
end;

procedure TLibVLC.libvlc_vlm_set_enabled(p_instance: Plibvlc_instance_t;
  psz_name: PAnsiChar; b_enabled: Integer; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_vlm_set_enabled) then
    Flibvlc_vlm_set_enabled(p_instance, psz_name, b_enabled, p_exception)
  else
    RaiseNotSupported('libvlc_vlm_set_enabled');
end;

procedure TLibVLC.libvlc_vlm_set_input(p_instance: Plibvlc_instance_t; psz_name,
  psz_input: PAnsiChar; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_vlm_set_input) then
    Flibvlc_vlm_set_input(p_instance, psz_name, psz_input, p_exception)
  else
    RaiseNotSupported('libvlc_vlm_set_input');
end;

procedure TLibVLC.libvlc_vlm_set_loop(p_instance: Plibvlc_instance_t;
  psz_name: PAnsiChar; b_loop: Integer; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_vlm_set_loop) then
    Flibvlc_vlm_set_loop(p_instance, psz_name, b_loop, p_exception)
  else
    RaiseNotSupported('libvlc_vlm_set_loop');
end;

procedure TLibVLC.libvlc_vlm_set_mux(p_instance: Plibvlc_instance_t; psz_name,
  psz_mux: PAnsiChar; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_vlm_set_mux) then
    Flibvlc_vlm_set_mux(p_instance, psz_name, psz_mux, p_exception)
  else
    RaiseNotSupported('libvlc_vlm_set_mux');
end;

procedure TLibVLC.libvlc_vlm_set_output(p_instance: Plibvlc_instance_t;
  psz_name, psz_output: PAnsiChar; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_vlm_set_output) then
    Flibvlc_vlm_set_output(p_instance, psz_name, psz_output, p_exception)
  else
    RaiseNotSupported('libvlc_vlm_set_output');
end;

function TLibVLC.libvlc_vlm_show_media(p_instance: Plibvlc_instance_t;
  psz_name: PAnsiChar; p_exception: Plibvlc_exception_t): PAnsiChar;
begin
  if Assigned(Flibvlc_vlm_show_media) then
    Result := Flibvlc_vlm_show_media(p_instance, psz_name, p_exception)
  else
    RaiseNotSupported('libvlc_vlm_show_media');
end;

procedure TLibVLC.libvlc_vlm_stop_media(p_instance: Plibvlc_instance_t;
  psz_name: PAnsiChar; p_exception: Plibvlc_exception_t);
begin
  if Assigned(Flibvlc_vlm_stop_media) then
    Flibvlc_vlm_stop_media(p_instance, psz_name, p_exception)
  else
    RaiseNotSupported('libvlc_vlm_stop_media');
end;

procedure TLibVLC.libvlc_wait(p_instance: Plibvlc_instance_t);
begin
  if Assigned(Flibvlc_wait) then
    Flibvlc_wait(p_instance)
  else
    RaiseNotSupported('libvlc_wait');
end;

procedure TLibVLC.LoadFunctions;
begin
  if FVersion >= EncodeVersion(1,0,5) then
  begin
    Flibvlc_exception_init := GetProcAddress(FLibrary, 'libvlc_exception_init');
    Flibvlc_exception_raised := GetProcAddress(FLibrary, 'libvlc_exception_raised');
    Flibvlc_exception_raise := GetProcAddress(FLibrary, 'libvlc_exception_raise');
    Flibvlc_exception_clear := GetProcAddress(FLibrary, 'libvlc_exception_clear');
    Flibvlc_exception_get_message := GetProcAddress(FLibrary, 'libvlc_exception_get_message');

    Flibvlc_new := GetProcAddress(FLibrary, 'libvlc_new');
    Flibvlc_release := GetProcAddress(FLibrary, 'libvlc_release');
    Flibvlc_retain := GetProcAddress(FLibrary, 'libvlc_retain');
    Flibvlc_add_intf := GetProcAddress(FLibrary, 'libvlc_add_intf');
    Flibvlc_wait := GetProcAddress(FLibrary, 'libvlc_wait');
    Flibvlc_get_version := GetProcAddress(FLibrary, 'libvlc_get_version');
    Flibvlc_get_compiler := GetProcAddress(FLibrary, 'libvlc_get_compiler');
    Flibvlc_get_changeset := GetProcAddress(FLibrary, 'libvlc_get_changeset');
    Flibvlc_free := GetProcAddress(FLibrary, 'libvlc_free');

    Flibvlc_event_attach := GetProcAddress(FLibrary, 'libvlc_event_attach');
    Flibvlc_event_detach := GetProcAddress(FLibrary, 'libvlc_event_detach');
    Flibvlc_event_type_name := GetProcAddress(FLibrary, 'libvlc_event_type_name');

    Flibvlc_get_log_verbosity := GetProcAddress(FLibrary, 'libvlc_get_log_verbosity');
    Flibvlc_set_log_verbosity := GetProcAddress(FLibrary, 'libvlc_set_log_verbosity');
    Flibvlc_log_open := GetProcAddress(FLibrary, 'libvlc_log_open');
    Flibvlc_log_close := GetProcAddress(FLibrary, 'libvlc_log_close');
    Flibvlc_log_count := GetProcAddress(FLibrary, 'libvlc_log_count');
    Flibvlc_log_clear := GetProcAddress(FLibrary, 'libvlc_log_clear');
    Flibvlc_log_get_iterator := GetProcAddress(FLibrary, 'libvlc_log_get_iterator');
    Flibvlc_log_iterator_free := GetProcAddress(FLibrary, 'libvlc_log_iterator_free');
    Flibvlc_log_iterator_has_next := GetProcAddress(FLibrary, 'libvlc_log_iterator_has_next');
    Flibvlc_log_iterator_next := GetProcAddress(FLibrary, 'libvlc_log_iterator_next');

    Flibvlc_media_new := GetProcAddress(FLibrary, 'libvlc_media_new');
    Flibvlc_media_new_as_node := GetProcAddress(FLibrary, 'libvlc_media_new_as_node');
    Flibvlc_media_add_option := GetProcAddress(FLibrary, 'libvlc_media_add_option');
    Flibvlc_media_add_option_untrusted := GetProcAddress(FLibrary, 'libvlc_media_add_option_untrusted');
    Flibvlc_media_retain := GetProcAddress(FLibrary, 'libvlc_media_retain');
    Flibvlc_media_release := GetProcAddress(FLibrary, 'libvlc_media_release');
    Flibvlc_media_get_mrl := GetProcAddress(FLibrary, 'libvlc_media_get_mrl');
    Flibvlc_media_duplicate := GetProcAddress(FLibrary, 'libvlc_media_duplicate');
    Flibvlc_media_get_meta := GetProcAddress(FLibrary, 'libvlc_media_get_meta');
    Flibvlc_media_get_state := GetProcAddress(FLibrary, 'libvlc_media_get_state');
    Flibvlc_media_subitems := GetProcAddress(FLibrary, 'libvlc_media_subitems');
    Flibvlc_media_event_manager := GetProcAddress(FLibrary, 'libvlc_media_event_manager');
    Flibvlc_media_get_duration := GetProcAddress(FLibrary, 'libvlc_media_get_duration');
    Flibvlc_media_is_preparsed := GetProcAddress(FLibrary, 'libvlc_media_is_preparsed');
    Flibvlc_media_set_user_data := GetProcAddress(FLibrary, 'libvlc_media_set_user_data');
    Flibvlc_media_get_user_data := GetProcAddress(FLibrary, 'libvlc_media_get_user_data');

    Flibvlc_media_list_new := GetProcAddress(FLibrary, 'libvlc_media_list_new');
    Flibvlc_media_list_release := GetProcAddress(FLibrary, 'libvlc_media_list_release');
    Flibvlc_media_list_retain := GetProcAddress(FLibrary, 'libvlc_media_list_retain');
    Flibvlc_media_list_set_media := GetProcAddress(FLibrary, 'libvlc_media_list_set_media');
    Flibvlc_media_list_media := GetProcAddress(FLibrary, 'libvlc_media_list_media');
    Flibvlc_media_list_add_media := GetProcAddress(FLibrary, 'libvlc_media_list_add_media');
    Flibvlc_media_list_insert_media := GetProcAddress(FLibrary, 'libvlc_media_list_insert_media');
    Flibvlc_media_list_remove_index := GetProcAddress(FLibrary, 'libvlc_media_list_remove_index');
    Flibvlc_media_list_count := GetProcAddress(FLibrary, 'libvlc_media_list_count');
    Flibvlc_media_list_item_at_index := GetProcAddress(FLibrary, 'libvlc_media_list_item_at_index');
    Flibvlc_media_list_index_of_item := GetProcAddress(FLibrary, 'libvlc_media_list_index_of_item');
    Flibvlc_media_list_is_readonly := GetProcAddress(FLibrary, 'libvlc_media_list_is_readonly');
    Flibvlc_media_list_lock := GetProcAddress(FLibrary, 'libvlc_media_list_lock');
    Flibvlc_media_list_unlock := GetProcAddress(FLibrary, 'libvlc_media_list_unlock');
    Flibvlc_media_list_flat_view := GetProcAddress(FLibrary, 'libvlc_media_list_flat_view');
    Flibvlc_media_list_hierarchical_view := GetProcAddress(FLibrary, 'libvlc_media_list_hierarchical_view');
    Flibvlc_media_list_hierarchical_node_view := GetProcAddress(FLibrary, 'libvlc_media_list_hierarchical_node_view');
    Flibvlc_media_list_event_manager := GetProcAddress(FLibrary, 'libvlc_media_list_event_manager');

    Flibvlc_media_list_view_retain := GetProcAddress(FLibrary, 'libvlc_media_list_view_retain');
    Flibvlc_media_list_view_release := GetProcAddress(FLibrary, 'libvlc_media_list_view_release');
    Flibvlc_media_list_view_event_manager := GetProcAddress(FLibrary, 'libvlc_media_list_view_event_manager');
    Flibvlc_media_list_view_count := GetProcAddress(FLibrary, 'libvlc_media_list_view_count');
    Flibvlc_media_list_view_item_at_index := GetProcAddress(FLibrary, 'libvlc_media_list_view_item_at_index');
    Flibvlc_media_list_view_children_at_index := GetProcAddress(FLibrary, 'libvlc_media_list_view_children_at_index');
    Flibvlc_media_list_view_children_for_item := GetProcAddress(FLibrary, 'libvlc_media_list_view_children_for_item');
    Flibvlc_media_list_view_index_of_item := GetProcAddress(FLibrary, 'libvlc_media_list_view_index_of_item');
    Flibvlc_media_list_view_insert_at_index := GetProcAddress(FLibrary, 'libvlc_media_list_view_insert_at_index');
    Flibvlc_media_list_view_remove_at_index := GetProcAddress(FLibrary, 'libvlc_media_list_view_remove_at_index');
    Flibvlc_media_list_view_add_item := GetProcAddress(FLibrary, 'libvlc_media_list_view_add_item');
    Flibvlc_media_list_view_parent_media_list := GetProcAddress(FLibrary, 'libvlc_media_list_view_parent_media_list');

    Flibvlc_media_library_new := GetProcAddress(FLibrary, 'libvlc_media_library_new');
    Flibvlc_media_library_release := GetProcAddress(FLibrary, 'libvlc_media_library_release');
    Flibvlc_media_library_retain := GetProcAddress(FLibrary, 'libvlc_media_library_retain');
    Flibvlc_media_library_load := GetProcAddress(FLibrary, 'libvlc_media_library_load');
    Flibvlc_media_library_save := GetProcAddress(FLibrary, 'libvlc_media_library_save');
    Flibvlc_media_library_media_list := GetProcAddress(FLibrary, 'libvlc_media_library_media_list');

    Flibvlc_media_player_new := GetProcAddress(FLibrary, 'libvlc_media_player_new');
    Flibvlc_media_player_new_from_media := GetProcAddress(FLibrary, 'libvlc_media_player_new_from_media');
    Flibvlc_media_player_release := GetProcAddress(FLibrary, 'libvlc_media_player_release');
    Flibvlc_media_player_retain := GetProcAddress(FLibrary, 'libvlc_media_player_retain');
    Flibvlc_media_player_set_media := GetProcAddress(FLibrary, 'libvlc_media_player_set_media');
    Flibvlc_media_player_get_media := GetProcAddress(FLibrary, 'libvlc_media_player_get_media');
    Flibvlc_media_player_event_manager := GetProcAddress(FLibrary, 'libvlc_media_player_event_manager');
    Flibvlc_media_player_is_playing := GetProcAddress(FLibrary, 'libvlc_media_player_is_playing');
    Flibvlc_media_player_play := GetProcAddress(FLibrary, 'libvlc_media_player_play');
    Flibvlc_media_player_pause := GetProcAddress(FLibrary, 'libvlc_media_player_pause');
    Flibvlc_media_player_stop := GetProcAddress(FLibrary, 'libvlc_media_player_stop');
    Flibvlc_media_player_set_nsobject := GetProcAddress(FLibrary, 'libvlc_media_player_set_nsobject');
    Flibvlc_media_player_get_nsobject := GetProcAddress(FLibrary, 'libvlc_media_player_get_nsobject');
    Flibvlc_media_player_set_agl := GetProcAddress(FLibrary, 'libvlc_media_player_set_agl');
    Flibvlc_media_player_get_agl := GetProcAddress(FLibrary, 'libvlc_media_player_get_agl');
    Flibvlc_media_player_set_xwindow := GetProcAddress(FLibrary, 'libvlc_media_player_set_xwindow');
    Flibvlc_media_player_get_xwindow := GetProcAddress(FLibrary, 'libvlc_media_player_get_xwindow');
    Flibvlc_media_player_set_hwnd := GetProcAddress(FLibrary, 'libvlc_media_player_set_hwnd');
    Flibvlc_media_player_get_hwnd := GetProcAddress(FLibrary, 'libvlc_media_player_get_hwnd');
    Flibvlc_media_player_get_length := GetProcAddress(FLibrary, 'libvlc_media_player_get_length');
    Flibvlc_media_player_get_time := GetProcAddress(FLibrary, 'libvlc_media_player_get_time');
    Flibvlc_media_player_set_time := GetProcAddress(FLibrary, 'libvlc_media_player_set_time');
    Flibvlc_media_player_get_position := GetProcAddress(FLibrary, 'libvlc_media_player_get_position');
    Flibvlc_media_player_set_position := GetProcAddress(FLibrary, 'libvlc_media_player_set_position');
    Flibvlc_media_player_set_chapter := GetProcAddress(FLibrary, 'libvlc_media_player_set_chapter');
    Flibvlc_media_player_get_chapter := GetProcAddress(FLibrary, 'libvlc_media_player_get_chapter');
    Flibvlc_media_player_get_chapter_count := GetProcAddress(FLibrary, 'libvlc_media_player_get_chapter_count');
    Flibvlc_media_player_will_play := GetProcAddress(FLibrary, 'libvlc_media_player_will_play');
    Flibvlc_media_player_get_chapter_count_for_title := GetProcAddress(FLibrary, 'libvlc_media_player_get_chapter_count_for_title');
    Flibvlc_media_player_set_title := GetProcAddress(FLibrary, 'libvlc_media_player_set_title');
    Flibvlc_media_player_get_title := GetProcAddress(FLibrary, 'libvlc_media_player_get_title');
    Flibvlc_media_player_get_title_count := GetProcAddress(FLibrary, 'libvlc_media_player_get_title_count');
    Flibvlc_media_player_previous_chapter := GetProcAddress(FLibrary, 'libvlc_media_player_previous_chapter');
    Flibvlc_media_player_next_chapter := GetProcAddress(FLibrary, 'libvlc_media_player_next_chapter');
    Flibvlc_media_player_get_rate := GetProcAddress(FLibrary, 'libvlc_media_player_get_rate');
    Flibvlc_media_player_set_rate := GetProcAddress(FLibrary, 'libvlc_media_player_set_rate');
    Flibvlc_media_player_get_state := GetProcAddress(FLibrary, 'libvlc_media_player_get_state');
    Flibvlc_media_player_get_fps := GetProcAddress(FLibrary, 'libvlc_media_player_get_fps');
    Flibvlc_media_player_has_vout := GetProcAddress(FLibrary, 'libvlc_media_player_has_vout');
    Flibvlc_media_player_is_seekable := GetProcAddress(FLibrary, 'libvlc_media_player_is_seekable');
    Flibvlc_media_player_can_pause := GetProcAddress(FLibrary, 'libvlc_media_player_can_pause');
    Flibvlc_track_description_release := GetProcAddress(FLibrary, 'libvlc_track_description_release');
    Flibvlc_toggle_fullscreen := GetProcAddress(FLibrary, 'libvlc_toggle_fullscreen');
    Flibvlc_set_fullscreen := GetProcAddress(FLibrary, 'libvlc_set_fullscreen');
    Flibvlc_get_fullscreen := GetProcAddress(FLibrary, 'libvlc_get_fullscreen');
    Flibvlc_video_get_height := GetProcAddress(FLibrary, 'libvlc_video_get_height');
    Flibvlc_video_get_width := GetProcAddress(FLibrary, 'libvlc_video_get_width');
    Flibvlc_video_get_scale := GetProcAddress(FLibrary, 'libvlc_video_get_scale');
    Flibvlc_video_set_scale := GetProcAddress(FLibrary, 'libvlc_video_set_scale');
    Flibvlc_video_get_aspect_ratio := GetProcAddress(FLibrary, 'libvlc_video_get_aspect_ratio');
    Flibvlc_video_set_aspect_ratio := GetProcAddress(FLibrary, 'libvlc_video_set_aspect_ratio');
    Flibvlc_video_get_spu := GetProcAddress(FLibrary, 'libvlc_video_get_spu');
    Flibvlc_video_get_spu_count := GetProcAddress(FLibrary, 'libvlc_video_get_spu_count');
    Flibvlc_video_get_spu_description := GetProcAddress(FLibrary, 'libvlc_video_get_spu_description');
    Flibvlc_video_set_spu := GetProcAddress(FLibrary, 'libvlc_video_set_spu');
    Flibvlc_video_set_subtitle_file := GetProcAddress(FLibrary, 'libvlc_video_set_subtitle_file');
    Flibvlc_video_get_title_description := GetProcAddress(FLibrary, 'libvlc_video_get_title_description');
    Flibvlc_video_get_chapter_description := GetProcAddress(FLibrary, 'libvlc_video_get_chapter_description');
    Flibvlc_video_get_crop_geometry := GetProcAddress(FLibrary, 'libvlc_video_get_crop_geometry');
    Flibvlc_video_set_crop_geometry := GetProcAddress(FLibrary, 'libvlc_video_set_crop_geometry');
    Flibvlc_toggle_teletext := GetProcAddress(FLibrary, 'libvlc_toggle_teletext');
    Flibvlc_video_get_teletext := GetProcAddress(FLibrary, 'libvlc_video_get_teletext');
    Flibvlc_video_set_teletext := GetProcAddress(FLibrary, 'libvlc_video_set_teletext');
    Flibvlc_video_get_track_count := GetProcAddress(FLibrary, 'libvlc_video_get_track_count');
    Flibvlc_video_get_track_description := GetProcAddress(FLibrary, 'libvlc_video_get_track_description');
    Flibvlc_video_get_track := GetProcAddress(FLibrary, 'libvlc_video_get_track');
    Flibvlc_video_set_track := GetProcAddress(FLibrary, 'libvlc_video_set_track');
    Flibvlc_video_take_snapshot := GetProcAddress(FLibrary, 'libvlc_video_take_snapshot');
    Flibvlc_audio_output_list_get := GetProcAddress(FLibrary, 'libvlc_audio_output_list_get');
    Flibvlc_audio_output_list_release := GetProcAddress(FLibrary, 'libvlc_audio_output_list_release');
    Flibvlc_audio_output_set := GetProcAddress(FLibrary, 'libvlc_audio_output_set');
    Flibvlc_audio_output_device_count := GetProcAddress(FLibrary, 'libvlc_audio_output_device_count');
    Flibvlc_audio_output_device_longname := GetProcAddress(FLibrary, 'libvlc_audio_output_device_longname');
    Flibvlc_audio_output_device_id := GetProcAddress(FLibrary, 'libvlc_audio_output_device_id');
    Flibvlc_audio_output_device_set := GetProcAddress(FLibrary, 'libvlc_audio_output_device_set');
    Flibvlc_audio_output_get_device_type := GetProcAddress(FLibrary, 'libvlc_audio_output_get_device_type');
    Flibvlc_audio_output_set_device_type := GetProcAddress(FLibrary, 'libvlc_audio_output_set_device_type');
    Flibvlc_audio_toggle_mute := GetProcAddress(FLibrary, 'libvlc_audio_toggle_mute');
    Flibvlc_audio_get_mute := GetProcAddress(FLibrary, 'libvlc_audio_get_mute');
    Flibvlc_audio_set_mute := GetProcAddress(FLibrary, 'libvlc_audio_set_mute');
    Flibvlc_audio_get_volume := GetProcAddress(FLibrary, 'libvlc_audio_get_volume');
    Flibvlc_audio_set_volume := GetProcAddress(FLibrary, 'libvlc_audio_set_volume');
    Flibvlc_audio_get_track_count := GetProcAddress(FLibrary, 'libvlc_audio_get_track_count');
    Flibvlc_audio_get_track_description := GetProcAddress(FLibrary, 'libvlc_audio_get_track_description');
    Flibvlc_audio_get_track := GetProcAddress(FLibrary, 'libvlc_audio_get_track');
    Flibvlc_audio_set_track := GetProcAddress(FLibrary, 'libvlc_audio_set_track');
    Flibvlc_audio_get_channel := GetProcAddress(FLibrary, 'libvlc_audio_get_channel');
    Flibvlc_audio_set_channel := GetProcAddress(FLibrary, 'libvlc_audio_set_channel');

    Flibvlc_media_list_player_new := GetProcAddress(FLibrary, 'libvlc_media_list_player_new');
    Flibvlc_media_list_player_release := GetProcAddress(FLibrary, 'libvlc_media_list_player_release');
    Flibvlc_media_list_player_set_media_player := GetProcAddress(FLibrary, 'libvlc_media_list_player_set_media_player');
    Flibvlc_media_list_player_set_media_list := GetProcAddress(FLibrary, 'libvlc_media_list_player_set_media_list');
    Flibvlc_media_list_player_play := GetProcAddress(FLibrary, 'libvlc_media_list_player_play');
    Flibvlc_media_list_player_pause := GetProcAddress(FLibrary, 'libvlc_media_list_player_pause');
    Flibvlc_media_list_player_is_playing := GetProcAddress(FLibrary, 'libvlc_media_list_player_is_playing');
    Flibvlc_media_list_player_get_state := GetProcAddress(FLibrary, 'libvlc_media_list_player_get_state');
    Flibvlc_media_list_player_play_item_at_index := GetProcAddress(FLibrary, 'libvlc_media_list_player_play_item_at_index');
    Flibvlc_media_list_player_play_item := GetProcAddress(FLibrary, 'libvlc_media_list_player_play_item');
    Flibvlc_media_list_player_stop := GetProcAddress(FLibrary, 'libvlc_media_list_player_stop');
    Flibvlc_media_list_player_next := GetProcAddress(FLibrary, 'libvlc_media_list_player_next');

    Flibvlc_media_discoverer_new_from_name := GetProcAddress(FLibrary, 'libvlc_media_discoverer_new_from_name');
    Flibvlc_media_discoverer_release := GetProcAddress(FLibrary, 'libvlc_media_discoverer_release');
    Flibvlc_media_discoverer_localized_name := GetProcAddress(FLibrary, 'libvlc_media_discoverer_localized_name');
    Flibvlc_media_discoverer_media_list := GetProcAddress(FLibrary, 'libvlc_media_discoverer_media_list');
    Flibvlc_media_discoverer_event_manager := GetProcAddress(FLibrary, 'libvlc_media_discoverer_event_manager');
    Flibvlc_media_discoverer_is_running := GetProcAddress(FLibrary, 'libvlc_media_discoverer_is_running');

    Flibvlc_vlm_release := GetProcAddress(FLibrary, 'libvlc_vlm_release');
    Flibvlc_vlm_add_broadcast := GetProcAddress(FLibrary, 'libvlc_vlm_add_broadcast');
    Flibvlc_vlm_add_vod := GetProcAddress(FLibrary, 'libvlc_vlm_add_vod');
    Flibvlc_vlm_del_media := GetProcAddress(FLibrary, 'libvlc_vlm_del_media');
    Flibvlc_vlm_set_enabled := GetProcAddress(FLibrary, 'libvlc_vlm_set_enabled');
    Flibvlc_vlm_set_output := GetProcAddress(FLibrary, 'libvlc_vlm_set_output');
    Flibvlc_vlm_set_input := GetProcAddress(FLibrary, 'libvlc_vlm_set_input');
    Flibvlc_vlm_add_input := GetProcAddress(FLibrary, 'libvlc_vlm_add_input');
    Flibvlc_vlm_set_loop := GetProcAddress(FLibrary, 'libvlc_vlm_set_loop');
    Flibvlc_vlm_set_mux := GetProcAddress(FLibrary, 'libvlc_vlm_set_mux');
    Flibvlc_vlm_change_media := GetProcAddress(FLibrary, 'libvlc_vlm_change_media');
    Flibvlc_vlm_play_media := GetProcAddress(FLibrary, 'libvlc_vlm_play_media');
    Flibvlc_vlm_stop_media := GetProcAddress(FLibrary, 'libvlc_vlm_stop_media');
    Flibvlc_vlm_pause_media := GetProcAddress(FLibrary, 'libvlc_vlm_pause_media');
    Flibvlc_vlm_seek_media := GetProcAddress(FLibrary, 'libvlc_vlm_seek_media');
    Flibvlc_vlm_show_media := GetProcAddress(FLibrary, 'libvlc_vlm_show_media');
    Flibvlc_vlm_get_media_instance_position := GetProcAddress(FLibrary, 'libvlc_vlm_get_media_instance_position');
    Flibvlc_vlm_get_media_instance_time := GetProcAddress(FLibrary, 'libvlc_vlm_get_media_instance_time');
    Flibvlc_vlm_get_media_instance_length := GetProcAddress(FLibrary, 'libvlc_vlm_get_media_instance_length');
    Flibvlc_vlm_get_media_instance_rate := GetProcAddress(FLibrary, 'libvlc_vlm_get_media_instance_rate');
    Flibvlc_vlm_get_media_instance_title := GetProcAddress(FLibrary, 'libvlc_vlm_get_media_instance_title');
    Flibvlc_vlm_get_media_instance_chapter := GetProcAddress(FLibrary, 'libvlc_vlm_get_media_instance_chapter');
    Flibvlc_vlm_get_media_instance_seekable := GetProcAddress(FLibrary, 'libvlc_vlm_get_media_instance_seekable');
  end;
end;

procedure TLibVLC.RaiseNotSupported(AFunction: String);
begin
  raise ELibVLCNotSupported.Create('"'+AFunction+'" is not supported by this libvcl');
end;

procedure TLibVLC.ReadVersion;
var
  get_ver_func : Tlibvlc_get_version;
  ver_str : String;
  p1, p2, p3 : Integer;

  procedure InvalidVersionException;
  begin
    raise Exception.Create('invalid libvlc version');
  end;
begin
  get_ver_func := GetProcAddress(FLibrary, 'libvlc_get_version');
  if not Assigned(get_ver_func) then
    Exception.Create('Can not read version')
  else
  begin
    ver_str := get_ver_func;
    p1 := Pos('.', ver_str);
    if p1 = 0 then
      InvalidVersionException;

    p2 := PosEx('.', ver_str, p1 + 1);
    if p2 = 0 then
      InvalidVersionException;

    p3 := PosEx(' ', ver_str, p2 + 1);
    if p3 = 0 then
      InvalidVersionException;

    FVersion := EncodeVersion(StrToInt(Copy(ver_str, 1, p1 - 1)),
                              StrToInt(Copy(ver_str, p1 + 1, p2 - p1 - 1)),
                              StrToInt(Copy(ver_str, p2 + 1, p3 - p2 - 1)));
  end;
end;

end.
