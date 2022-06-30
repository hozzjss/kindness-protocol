(impl-trait .sip-009.nft-trait)
;; kindness-protocol v1
;; A protocol for spreading kindness and gratitude.
;; through forming a network of community members 
;; Alice ==> (G1) grateful for ==> Bob
;; Bob ==> (G2) grateful for ==> Bob (advocate for yourself yo!!)
;; and thus forming a network of gratitude
;;

(define-non-fungible-token gratitude uint)


;; constants
;;
(define-constant ERR-NON-TRANSFERABLE u1001)
(define-constant ERR-RESTRICTED u1002)
(define-constant ERR-NOT-FOUND u404)
(define-constant ERR-NOT-AUTHORIZED u401)
(define-constant ERR-INVALID-MODE u403)
(define-constant ERR-ALREADY-FRIENDS u409)
(define-constant ERR-REQUEST-ALREADY-SENT u410)
(define-constant ERR-OUT-OF-BOUNDS-MF u411)
(define-constant PRIVACY-MODE-OPEN u0)
(define-constant PRIVACY-MODE-RESTRICTED u1)
;; data maps and vars
;;
(define-map nft-meta uint (string-ascii 59))
(define-map members principal {
    privacy-mode: uint
})
(define-map connection-requests { recipient: principal, index: uint } { sender: principal })
(define-map connection-requests-indexer principal uint)
;; to prevent spam
(define-map connection-requests-logger {sender: principal, recipient: principal} bool)
(define-map connections {sender: principal, recipient: principal} {blocked: bool})




(define-data-var last-token-id uint u0)
;; private functions
;;

;; public functions
;;

(define-read-only (get-last-token-id)
    (ok (var-get last-token-id))
)


(define-read-only (get-token-uri (nft-id uint))
    (match (map-get? nft-meta nft-id) uri 
        (ok (some (concat (concat "ipfs://" uri) "/metadata.json")))
        (err ERR-NOT-FOUND))
)

(define-read-only (get-multi-token-uri (nft-ids (list 200 uint))) 
    (ok (map get-token-uri nft-ids)))

(define-read-only (get-owner (nft-id uint))
    (ok (nft-get-owner? gratitude nft-id))
)

(define-read-only (is-member (account principal)) 
    (match (map-get? members account) status true false)
)

(define-read-only (is-valid-mode (mode uint)) 
    (or 
        (is-eq mode PRIVACY-MODE-OPEN)
        (is-eq mode PRIVACY-MODE-RESTRICTED)
    ))

(define-read-only (is-users-friends (account1 principal) (account2 principal)) 
    (or
        (not (default-to true (get blocked (map-get? connections {sender: account1, recipient: account2}))))
        (not (default-to true (get blocked (map-get? connections {sender: account2, recipient: account1}))))
    ))

(define-read-only (can-receive-gratitude (account principal)) 
    (match (map-get? members account)
        member-data
        (or
            (is-eq (get privacy-mode member-data) PRIVACY-MODE-OPEN)
            (is-users-friends tx-sender account)
        )
        true
    )
)

(define-public (transfer (nft-id uint) (sender principal) (recipient principal))
    (err ERR-NON-TRANSFERABLE)
)

(define-public (mint (nft-uri (string-ascii 59)) (recipient principal)) 
    (let 
        (
            (token-id (+ u1 (var-get last-token-id)))
        )
        (asserts! (is-eq tx-sender contract-caller) (err ERR-NOT-AUTHORIZED))
        (asserts! (can-receive-gratitude recipient) (err ERR-RESTRICTED))
        (unwrap-panic (if (is-member recipient) (ok true) (invite-member recipient)))
        (asserts! (is-member recipient) (err ERR-NOT-AUTHORIZED))
        (nft-mint? gratitude token-id recipient)
    )
)

(define-public (invite-member (account principal)) 
    (begin 
        (asserts! (is-eq tx-sender contract-caller) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-member tx-sender) (err ERR-NOT-AUTHORIZED))
        (asserts! (not (is-member account)) (err ERR-NOT-AUTHORIZED))
        (ok (map-insert members account {privacy-mode: PRIVACY-MODE-OPEN})))
)

(define-public (burn (nft-id uint)) 
    (begin 
        (asserts! (is-eq tx-sender contract-caller) (err ERR-NOT-AUTHORIZED))
        (asserts! (match (nft-get-owner? gratitude nft-id) owner 
            (is-eq tx-sender owner) false)
        (err ERR-NOT-AUTHORIZED))
        (nft-burn? gratitude nft-id tx-sender))
)

(define-public (set-privacy-mode (mode uint)) 
    (begin 
        (asserts! (is-eq tx-sender contract-caller) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-member tx-sender) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-valid-mode mode) (err ERR-INVALID-MODE))
        (map-set members tx-sender {privacy-mode: mode})
        (ok true))
)

(define-public (add-friend (account principal)) 
    (let 
        (
            (current-index (default-to u0 (map-get? connection-requests-indexer tx-sender)))
        )
        (asserts! (is-eq tx-sender contract-caller) (err ERR-NOT-AUTHORIZED))
        (asserts! (and (is-member account) (is-member tx-sender)) (err ERR-NOT-AUTHORIZED))
        (asserts! (not (is-users-friends account tx-sender)) (err ERR-ALREADY-FRIENDS))
        (asserts! (is-none (map-get? connection-requests-logger {sender: tx-sender, recipient: account})) (err ERR-REQUEST-ALREADY-SENT))
        (map-set connection-requests-indexer tx-sender (+ u1 current-index))
        (map-set connection-requests  {recipient: account, index: current-index} {sender: tx-sender})
        (ok true))
)

(define-public (accept-friend (account principal) (index uint)) 
    (let 
        (
            (current-index (default-to u0 (map-get? connection-requests-indexer tx-sender)))
        )
        (asserts! (is-eq tx-sender contract-caller) (err ERR-NOT-AUTHORIZED))
        (asserts! (and (is-member account) (is-member tx-sender)) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-some (map-get? connection-requests {recipient: account, index: index})) (err ERR-NOT-FOUND))
        (asserts! (not (is-users-friends account tx-sender)) (err ERR-ALREADY-FRIENDS))
        (asserts! (> current-index index) (err ERR-OUT-OF-BOUNDS-MF))
        (map-set connections {sender: account, recipient: tx-sender} {blocked: false})
        (map-delete connection-requests  {recipient: tx-sender, index: index})
        (map-delete connection-requests-logger {sender: account, recipient: tx-sender})
        (ok true))
)

(map-insert members tx-sender {privacy-mode: PRIVACY-MODE-OPEN})


