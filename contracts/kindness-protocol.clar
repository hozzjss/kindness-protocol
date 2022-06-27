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
(define-constant ERR-NOT-FOUND u404)
(define-constant ERR-NOT-AUTHORIZED u401)
;; data maps and vars
;;
(define-map nft-meta uint (string-ascii 59))
(define-map members principal bool)


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
    (match (map-get? members account) status status false)
)

(define-public (transfer (nft-id uint) (sender principal) (receiver principal))
    (err ERR-NON-TRANSFERABLE)
)

(define-public (mint (nft-uri (string-ascii 59)) (recipient principal)) 
    (let 
        (
            (token-id (+ u1 (var-get last-token-id)))
        )
        (asserts! (is-eq tx-sender contract-caller) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-member recipient) (err ERR-NOT-AUTHORIZED))
        (nft-mint? gratitude token-id recipient)
    )
)

(define-public (invite-member (account principal)) 
    (begin 
        (asserts! (is-eq tx-sender contract-caller) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-member account) (err ERR-NOT-AUTHORIZED))
        (ok (map-insert members account true)))
)

(define-public (burn (nft-id uint)) 
    (begin 
        (asserts! (is-eq tx-sender contract-caller) (err ERR-NOT-AUTHORIZED))
        (asserts! (match (nft-get-owner? gratitude nft-id) owner 
            (is-eq tx-sender owner) false)
        (err ERR-NOT-AUTHORIZED))
        (nft-burn? gratitude nft-id tx-sender))
)

(map-insert members tx-sender true)


