import { useState } from "react";

/**
 * Displays a random construction banner.
 * @returns a construction banner
 */
export default function Construction() {
  const [imageIndex] = useState(Math.floor(Math.random() * 5));
  const src = `/images/construction/long-${imageIndex}.gif`;

  return (
    <img
      src={src}
      title="Under construction!"
      alt='A 90s-looking "under construction" banner'
    />
  );
}
