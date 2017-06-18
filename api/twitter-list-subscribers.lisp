(in-package :twitter)


(define-command lists/subscribers (:get :cursor-user)
    (twitter-app-uri "lists/subscribers.json")
    "Returns the subscribers of the specified list."
  :list_id "Required: The numerical id of the list."
  :slug "Required: You can identify a list by its slug instead of its numerical id. You’ll also have to specify the list owner using the owner_id or owner_screen_name parameters."
  :owner_screen_name "Optional: The screen name of the user who owns the list being requested by a slug."
  :owner_id "Optional :The user ID of the user who owns the list being requested by a slug."
  :count "Optional: Specifies the number of results to return per page (see cursor below). The default is 20, with a maximum of 5,000."
  :cursor "Semi-optional: Causes the collection of list subscribers to be broken into “pages” of consistent sizes (specified by the count parameter)."
  :include_entities "Optional: The entities node will be disincluded when set to false."
  :skip_status "Optional: When set to either true, t or 1 statuses will not be included in the returned user objects.")

(define-twitter-method lists-subscribers (() &key (list-id nil) (slug nil) (owner-screen-name nil) (owner-id nil) (count nil) (cursor nil) (include-entities nil) (skip-status nil) ) :lists/subscribers )



(define-command lists/subscribers/show (:get :twitter-user)
    (twitter-app-uri "lists/subscribers/show.json")
    "Check if a user is a subscriber of the specified list."
  :list_id "Required : The id or slug of the list."
  :slug "Required: You can identify a list by its slug instead of its numerical id. If you decide to do so, note that you’ll also have to specify the list owner using the owner_id or owner_screen_name parameters."
  :user_id "Required : The ID of the user for whom to return results for."
  :screen_name "Required: The screen name of the user for whom to return results for."
  :owner_screen_name "Optional: The screen name of the user who owns the list being requested by a slug."
  :owner_id "Optional: The user ID of the user who owns the list being requested by a slug."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities.")

(define-twitter-method lists-subscribers-show (() &key (list-id nil) (slug nil) (user-id nil) (screen-name nil) (owner-screen-name nil) (owner-id nil) (include-entities nil)  ) :lists/subscribers/show)


(define-command lists/subscribers/destroy (:post :list-type)
    (twitter-app-uri "lists/subscribers/destroy.json")
    "Removes the subscriber from the specified list."
  :user_id "user" 
  :slug "Optional: You can identify a list by its slug instead of its numerical id."
  :list_id "Required : The id or slug of the list."
  :screen_name   "Optional: screen name of the user to be removed"
  :owner_screen_name "ptional: The screen name of the user who owns the list being requested by a slug"
  :owner_id "Optional: user ID of the user who owns the list being requested by a slug.")

(define-twitter-method lists-subscribers-destroy (() &key (user-id nil) (slug nil) (list-id nil) (screen-name nil) (owner-screen-name nil) (owner-id nil)) :lists/subscribers/destroy)



(define-command lists/subscribers/create (:post :list-type)
    (twitter-app-uri "lists/subscribers/create.json")
    "Add a subscriber to the specified list."
  :user_id "user" 
  :slug "Optional: You can identify a list by its slug instead of its numerical id."
  :list_id "Required : The id or slug of the list."
  :screen_name   "Optional: screen name of the user to be removed"
  :owner_screen_name "ptional: The screen name of the user who owns the list being requested by a slug"
  :owner_id "Optional: user ID of the user who owns the list being requested by a slug.")

(define-twitter-method lists-subscribers-create (() &key (user-id nil) (slug nil) (list-id nil) (screen-name nil) (owner-screen-name nil) (owner-id nil)) :lists/subscribers/create)




